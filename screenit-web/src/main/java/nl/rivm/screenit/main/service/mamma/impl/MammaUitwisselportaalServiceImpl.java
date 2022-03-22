package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.dao.mamma.MammaUitwisselportaalDao;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaUitwisselportaalServiceImpl implements MammaUitwisselportaalService
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private MammaBaseBeoordelingService beoordelingService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private MammaUitwisselportaalDao uitwisselportaalDao;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseScreeningrondeService screeningrondeService;

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
	public List<MammaDownloadOnderzoekenVerzoek> searchVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject, long first, long count, String sortProperty, boolean asc)
	{
		return uitwisselportaalDao.searchVerzoeken(searchObject, first, count, sortProperty, asc);
	}

	@Override
	public long countVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject)
	{
		return uitwisselportaalDao.countVerzoeken(searchObject);
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
	public List<MammaDownloadOnderzoekenVerzoek> getDownloadVerzoekenGedownload(MammaOnderzoek onderzoek)
	{
		return uitwisselportaalDao.getDownloadVerzoekenGedownload(onderzoek);
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
		return uitwisselportaalDao.getLaatstGedownloadDoorInstelling(dossier);
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
