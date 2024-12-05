package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.repository.mamma.MammaBaseOnderzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaMammografieRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.specification.mamma.MammaMammografieBaseSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.KeyValue;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HL7Exception;

import static nl.rivm.screenit.Constants.MAMMA_MAX_AANTAL_MAANDEN_GEEN_UITSLAG_ONDERBROKEN_ONDERZOEK;
import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;
import static nl.rivm.screenit.model.OrganisatieParameterKey.MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftDossierWatOvereenKomtMetRonde;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftIlmStatusBeschikbaarOfGeweest;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftMissendeUitslag;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftOnderzoekStatusNietOnderbroken;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftOnderzoekStatusOnderbroken;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftOnderzoekZonderUitslagBrieven;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class MammaBaseOnderzoekServiceImpl implements MammaBaseOnderzoekService
{
	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaMammografieRepository mammografieRepository;

	@Autowired
	@Lazy
	private MammaBaseAfspraakService afspraakService;

	@Autowired
	@Lazy
	private MammaBaseIlmService baseIlmService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseOnderzoekRepository onderzoekRepository;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Override
	public void onderzoekDoorvoerenVanuitSe(MammaOnderzoek onderzoek)
	{
		var afspraakStatus = onderzoek.getAfspraak().getStatus();
		var onderzoekStatus = onderzoek.getStatus();
		if ((MammaOnderzoekStatus.AFGEROND.equals(onderzoekStatus)
			|| MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoekStatus)
			|| MammaOnderzoekStatus.ONDERBROKEN.equals(onderzoekStatus))
			&& MammaAfspraakStatus.BEEINDIGD.equals(afspraakStatus))
		{
			onderzoekDoorvoeren(onderzoek);
			beeldenAlBeschikbaarControle(onderzoek);
			vervolgDoorgevoerdOnderzoek(onderzoek);
		}
		else
		{
			var errorMessage = String.format("Onderzoek (id: '%s') mag niet worden doorgevoerd, onderzoekstatus: %s; afspraakstatus: %s.", onderzoek.getId(),
				onderzoekStatus.name(),
				afspraakStatus.name());
			throw new IllegalStateException(errorMessage);
		}
	}

	private void vervolgDoorgevoerdOnderzoek(MammaOnderzoek onderzoek)
	{
		var heeftEerdereBeeldenBinnenRonde = heeftEerdereBeeldenBinnenRonde(onderzoek);
		var onderzoekOnvolledigZonderFotos = isOnderzoekOnvolledigZonderFotos(onderzoek);
		if (onderzoekOnvolledigZonderFotos && !heeftEerdereBeeldenBinnenRonde)
		{
			var screeningRonde = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
			briefService.maakBvoBrief(screeningRonde, BriefType.MAMMA_GEEN_ONDERZOEK);
			setScreeningrondeStatus(screeningRonde, ScreeningRondeStatus.AFGEROND);
			hibernateService.saveOrUpdate(screeningRonde);
		}
		else if (onderzoekOnvolledigZonderFotos)
		{
			onderzoekMetEerdereFotosDoorzetten(onderzoek);
		}
		else if (MammaOnderzoekStatus.AFGEROND.equals(onderzoek.getStatus()) || isOnderzoekOnvolledigMetFotos(onderzoek))
		{
			voegInitieleBeoordelingToe(onderzoek);
		}
	}

	private void onderzoekMetEerdereFotosDoorzetten(MammaOnderzoek onderzoek)
	{
		var voorgaandeOnderzoek = getAfsprakenMetAnderOnderzoekBinnenRonde(onderzoek)
			.map(MammaAfspraak::getOnderzoek)
			.max(Comparator.comparing(MammaOnderzoek::getCreatieDatum))
			.orElseThrow(() -> new IllegalStateException("Geen eerder onderzoek kunnen vinden"));
		var mammografie = voorgaandeOnderzoek.getMammografie();
		onderzoek.getMammografie().setIlmStatus(mammografie.getIlmStatus());
		onderzoek.getMammografie().setIlmStatusDatum(mammografie.getIlmStatusDatum());
		onderzoek.setStatus(MammaOnderzoekStatus.ONVOLLEDIG);
		onderzoek.setOnvolledigOnderzoek(OnvolledigOnderzoekOption.MET_FOTOS);
		hibernateService.saveOrUpdate(onderzoek);
		voegInitieleBeoordelingToe(onderzoek);
	}

	private boolean heeftEerdereBeeldenBinnenRonde(MammaOnderzoek onderzoek)
	{
		return getAfsprakenMetAnderOnderzoekBinnenRonde(onderzoek)
			.anyMatch(af -> MammaMammografieIlmStatus.BESCHIKBAAR.equals(af.getOnderzoek().getMammografie().getIlmStatus()));
	}

	private Stream<MammaAfspraak> getAfsprakenMetAnderOnderzoekBinnenRonde(MammaOnderzoek onderzoek)
	{
		return onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingen().stream().flatMap(mammaUitnodiging -> mammaUitnodiging.getAfspraken().stream())
			.filter(afspraak -> afspraak.getOnderzoek() != null && !afspraak.getOnderzoek().getId().equals(onderzoek.getId()));
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
	public void voegNieuweBeoordelingToe(MammaOnderzoek onderzoek)
	{
		if (onderzoek.getBeoordelingen().isEmpty())
		{
			throw new IllegalStateException("Onderzoek moet een beoordeling hebben. Onderzoek id: " + onderzoek.getId());
		}
		maakBeoordelingEnKoppelAanOnderzoek(onderzoek);
	}

	private MammaBeoordeling maakBeoordelingEnKoppelAanOnderzoek(MammaOnderzoek onderzoek)
	{
		var beoordeling = new MammaBeoordeling();
		beoordeling.setStatusDatum(currentDateSupplier.getDate());
		beoordeling.setStatus(MammaBeoordelingStatus.EERSTE_LEZING);
		beoordeling.setOpschortReden(MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN);
		beoordeling.setXdsVerslagStatus(XdsStatus.NIET_AANGEMELD);
		beoordeling.setOnderzoek(onderzoek);
		var beoordelingen = onderzoek.getBeoordelingen();
		beoordelingen.add(beoordeling);
		beoordeling.setBeoordelingsEenheid(bepaalBeVoorOnderzoek(onderzoek));
		onderzoek.setLaatsteBeoordeling(beoordeling);
		hibernateService.saveOrUpdateAll(onderzoek, beoordeling);
		return beoordeling;
	}

	private BeoordelingsEenheid bepaalBeVoorOnderzoek(MammaOnderzoek onderzoek)
	{
		var screeningsEenheid = onderzoek.getScreeningsEenheid();
		var beoordelingsEenheid = screeningsEenheid.getBeoordelingsEenheid();
		if (screeningsEenheid.getTijdelijkeBeoordelingsEenheid() != null && screeningsEenheid.getTijdelijkeBeTotEnMetDatum() != null
			&& screeningsEenheid.getTijdelijkeBeVanafDatum() != null)
		{
			var vanafDatum = DateUtil.toLocalDate(screeningsEenheid.getTijdelijkeBeVanafDatum());
			var totEnMetDatum = DateUtil.toLocalDate(screeningsEenheid.getTijdelijkeBeTotEnMetDatum());
			if (DateUtil.isWithinRange(vanafDatum, totEnMetDatum, DateUtil.toLocalDate(onderzoek.getCreatieDatum())))
			{
				beoordelingsEenheid = screeningsEenheid.getTijdelijkeBeoordelingsEenheid();
			}
		}
		return beoordelingsEenheid;
	}

	@Override
	public void ontvangBeeldenVoorOnderzoek(Client client, MammaScreeningRonde ronde, MammaOnderzoekType imsOnderzoekType) throws HL7Exception
	{
		var onderzoek = ronde.getLaatsteOnderzoek();
		var mammografie = onderzoek.getMammografie();

		if (imsOnderzoekType != onderzoek.getOnderzoekType())
		{
			var message = String.format("Onderzoek met uitnodigingsnummer %s is omgezet van '%s' naar '%s' op basis van het CentralAvailable bericht van het IMS.",
				ronde.getUitnodigingsNr(), onderzoek.getOnderzoekType().getNaam(), imsOnderzoekType.getNaam());

			onderzoek.setOnderzoekType(imsOnderzoekType);
			hibernateService.saveOrUpdate(onderzoek);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ONDERZOEKTYPE_GEWIJZIGD, client, message, Bevolkingsonderzoek.MAMMA);
		}

		if (mammografie.getIlmStatus() == MammaMammografieIlmStatus.NIET_BESCHIKBAAR)
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
		else if (mammografie.getIlmStatus() != MammaMammografieIlmStatus.BESCHIKBAAR)
		{
			var melding = String.format("Inkomend CA bericht voor ronde (id: '%s') is al verwerkt op %s en kon niet worden omgezet van %s naar BESCHIKBAAR",
				ronde.getId(), mammografie.getIlmStatusDatum(), mammografie.getIlmStatus().name());
			throw new HL7Exception(melding);
		}
	}

	@Override
	public void beeldenVerwijderdVoorOnderzoek(MammaIMSBericht bericht, Client client, boolean error)
	{
		var accessionNumber = bericht.getAccessionNumber();
		var mammografieen = getMammografienMetUitnodigingsNummer(accessionNumber);

		mammografieen.forEach(mammografie -> setMammografieStatus(mammografie, !error ? MammaMammografieIlmStatus.VERWIJDERD : MammaMammografieIlmStatus.VERWIJDEREN_MISLUKT));
		if (error)
		{
			var melding = String.format("Fout bij het verwijderen van beelden voor accession number %s. Raadpleeg het IMS systeem voor verdere analyse.", accessionNumber);
			var logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setMelding(melding);
			logEvent.setHl7MessageStructure(bericht.getHl7Bericht());
			logEvent.setLevel(Level.WARNING);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_ERROR_ONTVANGEN, logEvent, null, client, Bevolkingsonderzoek.MAMMA);
		}
		else
		{
			baseIlmService.verwijderIlmBezwaarPoging(client.getMammaDossier(), accessionNumber);
		}
	}

	private List<MammaMammografie> getMammografienMetUitnodigingsNummer(long accessionNumber)
	{
		return mammografieRepository.findAll(MammaMammografieBaseSpecification.heeftUitnodigingsNummer(accessionNumber));
	}

	private void beeldenAlBeschikbaarControle(MammaOnderzoek onderzoek)
	{
		if (isOnderzoekOnvolledigZonderFotos(onderzoek)
			&& onderzoekBeeldenBeschikbaar(onderzoek))
		{
			var client = onderzoek.getAfspraak().getUitnodiging().getBrief().getClient();
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ONVOLLEDIG_ZONDER_FOTOS_TOCH_IMS_BEELDEN, client,
				"Een onderzoek is automatisch omgezet van onvolledig zonder foto's naar onvolledig met foto's doordat er beelden beschikbaar zijn gesteld vanuit het IMS",
				Bevolkingsonderzoek.MAMMA);
			onderzoek.setOnvolledigOnderzoek(OnvolledigOnderzoekOption.MET_FOTOS);
			var screeningRonde = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
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
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean isOnderzoekOnvolledigZonderFotos(MammaOnderzoek onderzoek)
	{
		return MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoek.getStatus())
			&& OnvolledigOnderzoekOption.ZONDER_FOTOS.equals(onderzoek.getOnvolledigOnderzoek());
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
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
		else if (heeftEerdereBeeldenBinnenRonde(onderzoek))
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

		var dossier = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getDossier();
		var laatsteAfspraak = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			afspraakService.afspraakAnnuleren(laatsteAfspraak, MammaAfspraakStatus.GEANNULEERD_VIA_INFOLIJN, currentDateSupplier.getDate());
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<KeyValue> vorigeRondeTeksten(MammaOnderzoek onderzoek, boolean opSE)
	{
		var result = new ArrayList<KeyValue>();
		if (opSE)
		{
			addIfPresent("Extra MBB'er", naamExtraMedewerker(onderzoek), result);
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
		addIfPresent("Type onderzoek", onderzoek.getOnderzoekType().getNaam(), result);
		return result;
	}

	private String naamExtraMedewerker(MammaOnderzoek onderzoek)
	{
		var extraMedewerker = onderzoek.getExtraMedewerker();
		return extraMedewerker != null ? NaamUtil.getNaamGebruiker(extraMedewerker.getMedewerker()) : null;
	}

	private String operatiesTekst(MammaOnderzoek onderzoek)
	{
		if (Boolean.TRUE.equals(onderzoek.getOperatieLinks()))
		{
			return Boolean.TRUE.equals(onderzoek.getOperatieRechts()) ? "Rechts en links" : "Links";
		}
		else
		{
			return Boolean.TRUE.equals(onderzoek.getOperatieRechts()) ? "Rechts" : null;
		}
	}

	private void addIfPresent(String name, Object value, List<KeyValue> result)
	{
		if (value != null)
		{
			var valueString = value.toString().trim();
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
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
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
	}

	@Override
	public boolean forceerMammografieIlmStatus(long accessionNumber, MammaMammografieIlmStatus status, Account account)
	{
		var mammografieen = getMammografienMetUitnodigingsNummer(accessionNumber);
		var isChanged = new AtomicBoolean(false);
		mammografieen.forEach(mammografie ->
		{
			if (mammografie.getIlmStatus() != MammaMammografieIlmStatus.VERWIJDERD)
			{
				setMammografieStatus(mammografie, MammaMammografieIlmStatus.VERWIJDERD);
				isChanged.set(true);
			}
		});
		if (isChanged.get())
		{
			var melding = String.format("AccessionNumber: %d, status: %s, isBezwaar: %b, isUpload: %b", accessionNumber, status.toString(), false, false);
			var client = mammografieen.get(0).getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient();
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ILM_STATUS_GEFORCEERD, account, client, melding, Bevolkingsonderzoek.MAMMA);
		}
		return isChanged.get();
	}

	private void updateDossierMammografieVelden(MammaMammografie mammografie)
	{
		var afspraak = mammografie.getOnderzoek().getAfspraak();
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		dossier.setLaatsteMammografieAfgerond(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(dossier);
	}

	private boolean heeftBeelden(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && onderzoek.getMammografie() != null && onderzoek.getMammografie().getIlmStatus() == MammaMammografieIlmStatus.BESCHIKBAAR;
	}

	@Override
	public boolean heeftBinnenMammografieIntervalGeenOnderzoekGehad(MammaDossier dossier)
	{
		if (dossier.getLaatsteMammografieAfgerond() != null)
		{
			int minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
			var referentieDatum = DateUtil.toLocalDate(dossier.getLaatsteMammografieAfgerond());
			var minimaalIntervalOnderzoeken = referentieDatum.plusDays(minimaleIntervalMammografieOnderzoeken);
			return !minimaalIntervalOnderzoeken.isAfter(currentDateSupplier.getLocalDate());
		}
		return true;
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Optional<MammaOnderzoek> getLaatsteOnderzoekMetMissendeUitslagVanDossier(MammaDossier dossier)
	{
		return onderzoekRepository.findFirst(
			maakLaatsteOnderzoekMetMissendeUitslagSpecification()
				.and(heeftDossierWatOvereenKomtMetRonde(dossier)),
			Sort.by(Sort.Order.desc(MammaOnderzoek_.AFGEROND_OP)));
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Specification<MammaOnderzoek> getLaatsteOnderzoekMetMissendeUitslagSpecification()
	{
		return maakLaatsteOnderzoekMetMissendeUitslagSpecification();
	}

	private Specification<MammaOnderzoek> maakLaatsteOnderzoekMetMissendeUitslagSpecification()
	{
		var signaleringsTermijn = organisatieParameterService.getOrganisatieParameter(null, MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);
		var vandaag = currentDateSupplier.getLocalDate();

		var signalerenVanaf = vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		var minimaleSignaleringsDatum = vandaag.minusDays(signaleringsTermijn);

		var vandaagMinAantalMaandenGeenUitslag = vandaag.minusMonths(MAMMA_MAX_AANTAL_MAANDEN_GEEN_UITSLAG_ONDERBROKEN_ONDERZOEK);
		var minimaleSignaleringsDatumOnderbrokenOnderzoek = vandaag.minusDays(
			Math.min(signaleringsTermijn + ChronoUnit.DAYS.between(vandaagMinAantalMaandenGeenUitslag, vandaag),
				MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN.getMaxValue()));

		return heeftMissendeUitslag(signalerenVanaf)
			.and(heeftOnderzoekStatusNietOnderbroken(minimaleSignaleringsDatum)
				.or(heeftOnderzoekStatusOnderbroken(minimaleSignaleringsDatumOnderbrokenOnderzoek)))
			.and(heeftIlmStatusBeschikbaarOfGeweest())
			.and(heeftOnderzoekZonderUitslagBrieven())
			.and(heeftActieveClient());
	}
}
