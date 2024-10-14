package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekRepository;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekSpecification;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.OrganisatieMedewerker_;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaUitwisselportaalServiceImpl implements MammaUitwisselportaalService
{
	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	private final BerichtToBatchService berichtToBatchService;

	private final MammaBaseBeoordelingService beoordelingService;

	private final UploadDocumentService uploadDocumentService;

	private final MammaDownloadOnderzoekenVerzoekenDataProviderServiceImpl downloadOnderzoekenVerzoekenDataProviderService;

	private final MammaDownloadOnderzoekRepository downloadOnderzoekRepository;

	private final LogService logService;

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final MammaBaseBeoordelingService baseBeoordelingService;

	private static final OrganisatieType[] RADIOLOGIE_VERSLAG_ORGANISATIE_TYPES = new OrganisatieType[] { OrganisatieType.ZORGINSTELLING, OrganisatieType.MAMMAPOLI,
		OrganisatieType.RADIOLOGIEAFDELING };

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakDownloadVerzoek(List<MammaOnderzoek> onderzoeken, InstellingGebruiker loggedInInstellingGebruiker) throws IOException
	{
		Client client = beoordelingService.getClientVanBeoordeling(onderzoeken.get(0).getLaatsteBeoordeling());
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITWISSELPORTAAL_DOWNLOAD, loggedInInstellingGebruiker, client,
			"Downloadverzoek aangemaakt");
		MammaDownloadOnderzoekenVerzoek verzoek = new MammaDownloadOnderzoekenVerzoek();
		verzoek.setAangemaaktDoor(loggedInInstellingGebruiker);
		verzoek.setAangemaaktOp(dateSupplier.getDate());
		verzoek.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		verzoek.setGewijzigdOp(verzoek.getAangemaaktOp());
		hibernateService.saveOrUpdate(verzoek);
		for (MammaOnderzoek onderzoek : onderzoeken)
		{
			MammaDownloadOnderzoek downloadOnderzoek = new MammaDownloadOnderzoek();
			downloadOnderzoek.setOnderzoek(onderzoek);
			downloadOnderzoek.setVerzoek(verzoek);
			downloadOnderzoek.setStatus(BestandStatus.NOG_TE_VERWERKEN);
			verzoek.getOnderzoeken().add(downloadOnderzoek);
			hibernateService.saveOrUpdate(downloadOnderzoek);
		}
		hibernateService.saveOrUpdate(verzoek);
		createEmptyFile(verzoek);
	}

	private void createEmptyFile(MammaDownloadOnderzoekenVerzoek verzoek) throws IOException
	{
		File f = File.createTempFile("dummy", ".zip"); 
		UploadDocument document = new UploadDocument();
		document.setActief(true);
		document.setContentType("application/zip");
		document.setFile(f);

		String zipNaam = "onderzoekData-"
			+ verzoek.getOnderzoeken().get(0).getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon().getBsn()
			+ "-"
			+ new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS).format(new Date())
			+ ".zip";
		document.setNaam(zipNaam);
		uploadDocumentService.saveOrUpdate(document, FileStoreLocation.MAMMA_VERZAMELDE_ONDERZOEK_DATA, null, true);
		verzoek.setZipBestand(document);
		hibernateService.saveOrUpdate(verzoek);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void startDownloading()
	{
		berichtToBatchService.queueMammaVerzamelOnderzoeksDataBericht();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void resetDownloadVerzoek(MammaDownloadOnderzoekenVerzoek verzoek)
	{
		hibernateService.reload(verzoek);
		if (verzoek.getStatus() != BestandStatus.NOG_TE_VERWERKEN && verzoek.getStatus() != BestandStatus.BEZIG_MET_VERWERKEN)
		{
			verzoek.setStatus(BestandStatus.NOG_TE_VERWERKEN);
			verzoek.setGewijzigdOp(dateSupplier.getDate());
			hibernateService.saveOrUpdate(verzoek);
			for (MammaDownloadOnderzoek onderzoek : verzoek.getOnderzoeken())
			{
				onderzoek.setStatus(BestandStatus.NOG_TE_VERWERKEN);
				onderzoek.setStatusMelding(null);
				hibernateService.saveOrUpdate(onderzoek);
			}
		}
	}

	@Override
	public Optional<MammaDownloadOnderzoekenVerzoek> geldigDownloadVerzoekVoorIngelogdeGebruiker(long downloadVerzoekId, InstellingGebruiker instellingGebruiker)
	{
		if (downloadVerzoekId <= 0 || instellingGebruiker == null)
		{
			return Optional.empty();
		}

		var verzoekFilter = maakDownloadVerzoekFilter(instellingGebruiker);
		verzoekFilter.setId(downloadVerzoekId);

		if (downloadOnderzoekenVerzoekenDataProviderService.size(verzoekFilter) != 1)
		{
			return Optional.empty();
		}

		var downloadVerzoek = hibernateService.load(MammaDownloadOnderzoekenVerzoek.class, downloadVerzoekId);

		return zipKanGedownloadWorden(downloadVerzoek) ? Optional.of(downloadVerzoek) : Optional.empty();
	}

	@Override
	public MammaDownloadOnderzoekenVerzoek maakDownloadVerzoekFilter(InstellingGebruiker instellingGebruiker)
	{
		var verzoekFilter = new MammaDownloadOnderzoekenVerzoek();
		if (instellingGebruiker.getOrganisatie().getOrganisatieType() != OrganisatieType.RIVM)
		{
			verzoekFilter.setAangemaaktDoor(instellingGebruiker);
		}
		return verzoekFilter;
	}

	@Override
	public boolean zipKanGedownloadWorden(MammaDownloadOnderzoekenVerzoek downloadOnderzoekenVerzoek)
	{
		var status = downloadOnderzoekenVerzoek.getStatus();
		return (status == BestandStatus.VERWERKT || status == BestandStatus.CRASH) && !heeftClientBezwaarTegenUitwisseling(downloadOnderzoekenVerzoek);
	}

	private boolean heeftClientBezwaarTegenUitwisseling(MammaDownloadOnderzoekenVerzoek downloadOnderzoekenVerzoek)
	{
		if (downloadOnderzoekenVerzoek.getOnderzoeken() != null && !downloadOnderzoekenVerzoek.getOnderzoeken().isEmpty())
		{
			Client client = baseBeoordelingService.getClientVanBeoordeling(downloadOnderzoekenVerzoek.getOnderzoeken().get(0).getOnderzoek().getLaatsteBeoordeling());
			return BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS, Bevolkingsonderzoek.MAMMA);
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateDownloadVerzoekInformatie(MammaDownloadOnderzoekenVerzoek verzoek, InstellingGebruiker loggedInInstellingGebruiker)
	{
		verzoek.setGedownloadOp(dateSupplier.getDate());
		hibernateService.saveOrUpdate(verzoek);
		MammaOnderzoek onderzoek = verzoek.getOnderzoeken().get(0).getOnderzoek();
		MammaScreeningRonde laatsteScreeningRondeMetUitslag = screeningrondeService
			.getLaatsteScreeningRondeMetUitslag(onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient());
		if (getFollowUpRadiologieVerslag(laatsteScreeningRondeMetUitslag, loggedInInstellingGebruiker) == null
			&& ArrayUtils.contains(RADIOLOGIE_VERSLAG_ORGANISATIE_TYPES, loggedInInstellingGebruiker.getOrganisatie().getOrganisatieType()))
		{
			MammaFollowUpRadiologieVerslag radiologieVerslag = new MammaFollowUpRadiologieVerslag();
			laatsteScreeningRondeMetUitslag.getFollowUpRadiologieVerslagen().add(radiologieVerslag);
			radiologieVerslag.setScreeningRonde(laatsteScreeningRondeMetUitslag);
			radiologieVerslag.setAangemaaktIn(loggedInInstellingGebruiker.getOrganisatie());
			radiologieVerslag.setAangemaaktOp(dateSupplier.getDate());
			radiologieVerslag.setInformatieBeschikbaar(true);
			hibernateService.saveOrUpdateAll(radiologieVerslag, laatsteScreeningRondeMetUitslag);
		}
		Client client = beoordelingService.getClientVanBeoordeling(onderzoek.getLaatsteBeoordeling());
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_UITWISSELPORTAAL_DOWNLOAD, loggedInInstellingGebruiker, client,
			"ZIP bestand is gedownload");
	}

	@Override
	public MammaFollowUpRadiologieVerslag getFollowUpRadiologieVerslag(MammaScreeningRonde screeningRonde, InstellingGebruiker loggedInInstellingGebruiker)
	{
		return screeningRonde != null
			? screeningRonde.getFollowUpRadiologieVerslagen().stream()
			.filter(mammaFollowUpRadiologieVerslag -> mammaFollowUpRadiologieVerslag.getAangemaaktIn().equals(loggedInInstellingGebruiker.getOrganisatie())).findFirst()
			.orElse(null)
			: null;
	}

	@Override
	public Instelling getLaatstGedownloadDoorInstelling(MammaDossier dossier)
	{
		return downloadOnderzoekRepository.findWith(MammaDownloadOnderzoekSpecification.isGedownload().and(MammaDownloadOnderzoekSpecification.heeftDossier(dossier))
				.and(MammaDownloadOnderzoekSpecification.isGemaaktDoorActieveInstelling()), Instelling.class,
			q -> q.projection((cb, r) ->
				{
					var verzoekJoin = join(r, MammaDownloadOnderzoek_.verzoek);
					var aangemaaktDoorJoin = join(verzoekJoin, MammaDownloadOnderzoekenVerzoek_.aangemaaktDoor);
					return aangemaaktDoorJoin.get(OrganisatieMedewerker_.organisatie);
				})
				.sortBy(Sort.by(Sort.Order.desc(propertyChain(MammaDownloadOnderzoek_.VERZOEK, MammaDownloadOnderzoekenVerzoek_.GEDOWNLOAD_OP))))
				.first().orElse(null));
	}

	@Override
	public List<MammaScreeningRonde> beschikbareRondesVoorDownload(Client client)
	{
		return new ArrayList<>(client.getMammaDossier().getScreeningRondes()) 
			.stream()
			.filter(this::rondeBeschikbaarVoorDownload)
			.sorted(Comparator.comparing(MammaScreeningRonde::getCreatieDatum).reversed())
			.collect(Collectors.toList());
	}

	private boolean rondeBeschikbaarVoorDownload(MammaScreeningRonde ronde)
	{
		var laatsteOnderzoek = ronde.getLaatsteOnderzoek();
		return laatsteOnderzoek != null && laatsteOnderzoek.getLaatsteBeoordeling() != null &&
			MammaMammografieIlmStatus.beeldenBeschikbaar(laatsteOnderzoek.getMammografie().getIlmStatus()) &&
			Arrays.asList(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG, MammaBeoordelingStatus.UITSLAG_GUNSTIG).contains(laatsteOnderzoek.getLaatsteBeoordeling().getStatus());
	}
}
