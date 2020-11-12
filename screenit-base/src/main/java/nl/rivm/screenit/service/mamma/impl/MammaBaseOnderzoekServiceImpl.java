package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.mamma.MammaBaseOnderzoekDao;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.KeyValue;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HL7Exception;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseOnderzoekServiceImpl implements MammaBaseOnderzoekService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseOnderzoekServiceImpl.class);

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseOnderzoekDao baseOnderzoekDao;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	@Override
	public void onderzoekDoorvoerenVanuitSe(MammaOnderzoek onderzoek)
	{
		MammaAfspraakStatus afspraakStatus = onderzoek.getAfspraak().getStatus();
		MammaOnderzoekStatus onderzoekStatus = onderzoek.getStatus();
		if ((MammaOnderzoekStatus.AFGEROND.equals(onderzoekStatus)
			|| MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoekStatus)
			|| MammaOnderzoekStatus.ONDERBROKEN.equals(onderzoekStatus))
			&& MammaAfspraakStatus.BEEINDIGD.equals(afspraakStatus))
		{
			onderzoekDoorvoeren(onderzoek);
			beeldenAlBeschikbaarControle(onderzoek);

			boolean heeftEerdereBeeldenBinnenUitnodiging = heeftEerdereBeeldenBinnenUitnodiging(onderzoek);
			boolean onderzoekOnvolledigZonderFotos = isOnderzoekOnvolledigZonderFotos(onderzoek);
			if (onderzoekOnvolledigZonderFotos && !heeftEerdereBeeldenBinnenUitnodiging)
			{
				MammaScreeningRonde screeningRonde = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
				briefService.maakMammaBrief(screeningRonde, BriefType.MAMMA_GEEN_ONDERZOEK);
				setScreeningrondeStatus(screeningRonde, ScreeningRondeStatus.AFGEROND);
				hibernateService.saveOrUpdate(screeningRonde);
			}
			else if (onderzoekOnvolledigZonderFotos)
			{
				onderzoekMetEerdereFotosDoorzetten(onderzoek);
			}
			else if (MammaOnderzoekStatus.AFGEROND.equals(onderzoekStatus) || isOnderzoekOnvolledigMetFotos(onderzoek))
			{
				voegInitieleBeoordelingToe(onderzoek);
			}
		}
		else
		{
			String errorMessage = String.format("Onderzoek mag niet worden doorgevoerd, onderzoekstatus: %s; afspraakstatus: %s.", onderzoekStatus.name(),
				afspraakStatus.name());
			throw new IllegalStateException(errorMessage);
		}
	}

	private void onderzoekMetEerdereFotosDoorzetten(MammaOnderzoek onderzoek)
	{
		MammaOnderzoek voorgaandeOnderzoek = onderzoek.getAfspraak().getUitnodiging().getAfspraken().stream()
			.filter(a -> !a.getOnderzoek().getId().equals(onderzoek.getId()))
			.map(MammaAfspraak::getOnderzoek)
			.max(Comparator.comparing(MammaOnderzoek::getCreatieDatum))
			.orElseThrow(() -> new IllegalStateException("Geen eerder onderzoek kunnen vinden"));
		MammaMammografie mammografie = voorgaandeOnderzoek.getMammografie();
		onderzoek.getMammografie().setIlmStatus(mammografie.getIlmStatus());
		onderzoek.getMammografie().setIlmStatusDatum(mammografie.getIlmStatusDatum());
		onderzoek.setStatus(MammaOnderzoekStatus.ONVOLLEDIG);
		onderzoek.setOnvolledigOnderzoek(OnvolledigOnderzoekOption.MET_FOTOS);
		hibernateService.saveOrUpdate(onderzoek);
		voegInitieleBeoordelingToe(onderzoek);
	}

	public boolean heeftEerdereBeeldenBinnenUitnodiging(MammaOnderzoek onderzoek)
	{

		return onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingen().stream().flatMap(mammaUitnodiging -> mammaUitnodiging.getAfspraken().stream())
			.filter(afspraak -> afspraak.getOnderzoek() != null && !afspraak.getOnderzoek().getId().equals(onderzoek.getId()))
			.anyMatch(af -> MammaMammografieIlmStatus.BESCHIKBAAR.equals(af.getOnderzoek().getMammografie().getIlmStatus()));
	}

	@Override
	public MammaBeoordeling voegInitieleBeoordelingToe(MammaOnderzoek onderzoek)
	{
		if (!onderzoek.getBeoordelingen().isEmpty())
		{
			throw new IllegalStateException("Onderzoek mag nog geen beoordelingen hebben. Onderzoek id: " + onderzoek.getId());
		}
		return maakBeoordelingEnKoppelAanOnderzoek(onderzoek);
	}

	@Override
	public MammaBeoordeling voegNieuweBeoordelingToe(MammaOnderzoek onderzoek)
	{
		if (onderzoek.getBeoordelingen().isEmpty())
		{
			throw new IllegalStateException("Onderzoek moet een beoordeling hebben. Onderzoek id: " + onderzoek.getId());
		}
		return maakBeoordelingEnKoppelAanOnderzoek(onderzoek);
	}

	private MammaBeoordeling maakBeoordelingEnKoppelAanOnderzoek(MammaOnderzoek onderzoek)
	{
		MammaBeoordeling beoordeling = new MammaBeoordeling();
		beoordeling.setStatusDatum(currentDateSupplier.getDate());
		beoordeling.setStatus(MammaBeoordelingStatus.EERSTE_LEZING);
		beoordeling.setOpschortReden(MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN);
		beoordeling.setXdsVerslagStatus(XdsStatus.NIET_AANGEMELD);
		beoordeling.setOnderzoek(onderzoek);
		List<MammaBeoordeling> beoordelingen = onderzoek.getBeoordelingen();
		beoordelingen.add(beoordeling);
		beoordeling.setBeoordelingsEenheid(bepaalBeVoorOnderzoek(onderzoek));
		onderzoek.getBeoordelingen().add(beoordeling);
		onderzoek.setLaatsteBeoordeling(beoordeling);
		hibernateService.saveOrUpdateAll(onderzoek, beoordeling);
		return beoordeling;
	}

	private BeoordelingsEenheid bepaalBeVoorOnderzoek(MammaOnderzoek onderzoek)
	{
		MammaScreeningsEenheid screeningsEenheid = onderzoek.getScreeningsEenheid();
		BeoordelingsEenheid beoordelingsEenheid = screeningsEenheid.getBeoordelingsEenheid();
		if (screeningsEenheid.getTijdelijkeBeoordelingsEenheid() != null && screeningsEenheid.getTijdelijkeBeTotEnMetDatum() != null
			&& screeningsEenheid.getTijdelijkeBeVanafDatum() != null)
		{
			LocalDate vanafDatum = DateUtil.toLocalDate(screeningsEenheid.getTijdelijkeBeVanafDatum());
			LocalDate totEnMetDatum = DateUtil.toLocalDate(screeningsEenheid.getTijdelijkeBeTotEnMetDatum());
			if (DateUtil.isWithinRange(vanafDatum, totEnMetDatum, DateUtil.toLocalDate(onderzoek.getCreatieDatum())))
			{
				beoordelingsEenheid = screeningsEenheid.getTijdelijkeBeoordelingsEenheid();
			}
		}
		return beoordelingsEenheid;
	}

	@Override
	public void ontvangBeeldenVoorOnderzoek(Client client, MammaScreeningRonde ronde) throws HL7Exception
	{
		MammaOnderzoek onderzoek = ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
		MammaMammografie mammografie = onderzoek.getMammografie();
		if (MammaMammografieIlmStatus.NIET_BESCHIKBAAR.equals(mammografie.getIlmStatus()))
		{
			updateDossierMammografieVelden(mammografie);
			setMammografieStatus(mammografie, MammaMammografieIlmStatus.BESCHIKBAAR);
			if (isOnderzoekOnvolledigZonderFotos(onderzoek))
			{
				if (onderzoek.isDoorgevoerd())
				{
					voegInitieleBeoordelingToe(onderzoek);
					LOG.info("Beoordeling aangemaakt voor onvolledig onderzoek {}, client id: {}", onderzoek.getId(), client.getId());
				}
				beeldenAlBeschikbaarControle(onderzoek);
			}
		}
		else
		{
			String melding = String.format("Inkomend CA bericht voor uitnodgingsnr %s is al verwerkt op %s en kon niet worden omgezet van %s naar BESCHIKBAAR",
				ronde.getUitnodigingsNr(), mammografie.getIlmStatusDatum(), mammografie.getIlmStatus().name());
			throw new HL7Exception(melding);
		}
	}

	@Override
	public void beeldenVerwijderdVoorOnderzoek(MammaIMSBericht bericht, Client client, boolean error)
	{
		Long accessionNumber = bericht.getAccessionNumber();
		List<MammaOnderzoek> onderzoeken = baseOnderzoekDao.getOnderzoekenVanUitnodigingsNummerMetBeeldenTeVerwijderen(accessionNumber);

		onderzoeken.forEach(onderzoek -> {
			MammaMammografie mammografie = onderzoek.getMammografie();
			setMammografieStatus(mammografie, !error ? MammaMammografieIlmStatus.VERWIJDERD : MammaMammografieIlmStatus.VERWIJDEREN_MISLUKT);
		});
		if (error)
		{
			String melding = String.format("Fout bij het verwijderen van beelden voor accession number %s. Raadpleeg het IMS systeem voor verdere analyse.", accessionNumber);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_ERROR_ONTVANGEN, client, melding, Bevolkingsonderzoek.MAMMA);
		}
	}

	private void beeldenAlBeschikbaarControle(MammaOnderzoek onderzoek)
	{
		if (isOnderzoekOnvolledigZonderFotos(onderzoek)
			&& onderzoekBeeldenBeschikbaar(onderzoek))
		{
			Client client = onderzoek.getAfspraak().getUitnodiging().getBrief().getClient();
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ONVOLLEDIG_ZONDER_FOTOS_TOCH_IMS_BEELDEN, client,
				"Een onderzoek is automatisch omgezet van onvolledig zonder foto's naar onvolledig met foto's doordat er beelden beschikbaar zijn gesteld vanuit het IMS",
				Bevolkingsonderzoek.MAMMA);
			onderzoek.setOnvolledigOnderzoek(OnvolledigOnderzoekOption.MET_FOTOS);
			MammaScreeningRonde screeningRonde = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
			setScreeningrondeStatus(screeningRonde, ScreeningRondeStatus.LOPEND);
			hibernateService.saveOrUpdateAll(screeningRonde, onderzoek);
			LOG.info("Onvolledig onderzoek zonder foto's omgezet naar met foto's voor onderzoek id: {}, client id: {}", onderzoek.getId(), client.getId());
		}
	}

	private void setScreeningrondeStatus(MammaScreeningRonde screeningRonde, ScreeningRondeStatus lopend)
	{
		screeningRonde.setStatus(lopend);
		screeningRonde.setStatusDatum(currentDateSupplier.getDate());
	}

	private boolean onderzoekBeeldenBeschikbaar(MammaOnderzoek onderzoek)
	{
		return MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(onderzoek.getMammografie().getIlmStatus());
	}

	@Override
	public boolean isOnderzoekOnvolledigZonderFotos(MammaOnderzoek onderzoek)
	{
		return MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoek.getStatus())
			&& OnvolledigOnderzoekOption.ZONDER_FOTOS.equals(onderzoek.getOnvolledigOnderzoek());
	}

	@Override
	public boolean isOnderzoekOnvolledigMetFotos(MammaOnderzoek onderzoek)
	{
		return MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoek.getStatus()) &&
			OnvolledigOnderzoekOption.MET_FOTOS.equals(onderzoek.getOnvolledigOnderzoek());
	}

	private void onderzoekDoorvoeren(MammaOnderzoek onderzoek)
	{
		onderzoek.setDoorgevoerd(true);
		hibernateService.saveOrUpdate(onderzoek);
	}

	@Override
	public void vervolgOnderbrokenOnderzoeken(MammaOnderzoek onderzoek)
	{
		if (onderzoekBeeldenBeschikbaar(onderzoek))
		{
			onderzoek.setStatus(MammaOnderzoekStatus.ONVOLLEDIG);
			onderzoek.setOnvolledigOnderzoek(OnvolledigOnderzoekOption.MET_FOTOS);
			maakBeoordelingEnKoppelAanOnderzoek(onderzoek);
			annuleerAfspraak(onderzoek);
		}
		else if (heeftEerdereBeeldenBinnenUitnodiging(onderzoek))
		{
			onderzoekMetEerdereFotosDoorzetten(onderzoek);
			annuleerAfspraak(onderzoek);
		}
		else
		{
			onderzoek.setStatus(MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG);
		}

		hibernateService.saveOrUpdate(onderzoek);
	}

	private void annuleerAfspraak(MammaOnderzoek onderzoek)
	{

		MammaDossier dossier = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getDossier();
		MammaAfspraak laatsteAfspraak = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			afspraakService.afspraakAnnuleren(laatsteAfspraak, MammaAfspraakStatus.GEANNULEERD_VIA_INFOLIJN, currentDateSupplier.getDate());
		}
	}

	@Override
	public List<KeyValue> vorigeRondeTeksten(MammaOnderzoek onderzoek, boolean opSE)
	{
		List<KeyValue> result = new ArrayList<>();
		if (opSE)
		{
			addIfPresent("Extra MBB'er", onderzoek.getExtraMedewerker(), result);
			addIfPresent("Opmerking MBB'er", onderzoek.getOpmerkingMbber(), result);
		}
		addIfPresent("Extra foto's", StringUtil.literals2string(onderzoek.getExtraFotosRedenen()), result);
		if (onderzoek.getEerderMammogramZorginstelling() != null)
		{
			addIfPresent("Zorginstelling eerder mammogram", onderzoek.getEerderMammogramZorginstelling().getNaam(), result);
		}
		addIfPresent("Jaartal eerder mammogram", onderzoek.getEerderMammogramJaartal(), result);
		if (onderzoek.getSuboptimaleInsteltechniek() != null)
		{
			addIfPresent("Suboptimale insteltechniek", onderzoek.getSuboptimaleInsteltechniek().getNaam(), result);
		}
		if (opSE)
		{
			addIfPresent("Reden fotobespreking", StringUtil.literal2string(onderzoek.getRedenFotobespreking()), result);
		}
		addIfPresent("Opmerking voor radioloog", onderzoek.getOpmerkingVoorRadioloog(), result);
		addIfPresent("Operatie(s)", operatiesTekst(onderzoek), result);
		addIfPresent("Aanvullende informatie", onderzoek.getAanvullendeInformatieOperatie(), result);
		addIfPresent("Onvolledig onderzoek", onderzoek.getOnvolledigOnderzoek() != null ? onderzoek.getOnvolledigOnderzoek().getNaam() : null, result);
		addIfPresent("Onderbroken onderzoek", onderzoek.getOnderbrokenOnderzoek() != null ? onderzoek.getOnderbrokenOnderzoek().getNaam() : null, result);
		return result;
	}

	private String operatiesTekst(MammaOnderzoek onderzoek)
	{
		if (onderzoek.getOperatieLinks())
		{
			return onderzoek.getOperatieRechts() ? "Rechts en links" : "Links";
		}
		else
		{
			return onderzoek.getOperatieRechts() ? "Rechts" : null;
		}
	}

	private void addIfPresent(String name, Object value, List<KeyValue> result)
	{
		if (value != null)
		{
			String valueString = value.toString().trim();
			if (!valueString.isEmpty())
			{
				result.add(new KeyValue(name, valueString));
			}
		}
	}

	@Override
	public void setMammografieStatus(MammaMammografie mammografie, MammaMammografieIlmStatus status)
	{
		mammografie.setIlmStatus(status);
		mammografie.setIlmStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(mammografie);
	}

	@Override
	public List<MammaOnderzoek> getOnderzoekenMetBeelden(Client client)
	{
		if (client.getMammaDossier() != null)
		{
			return client.getMammaDossier().getScreeningRondes().stream()
				.map(MammaScreeningRondeUtil::getLaatsteOnderzoek)
				.filter(this::heeftBeelden)
				.sorted(Comparator.comparing(MammaOnderzoek::getCreatieDatum).reversed())
				.collect(Collectors.toList());
		}
		else
		{
			return new ArrayList<>();
		}
	};

	private void updateDossierMammografieVelden(MammaMammografie mammografie)
	{
		MammaAfspraak afspraak = mammografie.getOnderzoek().getAfspraak();
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		dossier.setLaatsteMammografieAfgerond(currentDateSupplier.getDate());
		if (dossier.getEersteMammografieAfgerondStandplaatsRonde() == null)
		{
			dossier.setEersteMammografieAfgerondStandplaatsRonde(afspraak.getStandplaatsPeriode().getStandplaatsRonde());
		}
		hibernateService.saveOrUpdate(dossier);
	}

	private boolean heeftBeelden(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && onderzoek.getMammografie() != null && onderzoek.getMammografie().getIlmStatus() == MammaMammografieIlmStatus.BESCHIKBAAR;
	}
}
