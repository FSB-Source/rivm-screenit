package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.comparator.BezwaarComparator;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.exceptions.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.NieuweIntakeAfspraakMakenReden;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.NieuweIfobtResultaat;
import nl.rivm.screenit.model.logging.NieuweIFobtAanvraagLogEvent;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonHuisartsService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Hibernate;
import org.hibernate.exception.GenericJDBCException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateJdbcException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ClientContactServiceImpl implements ClientContactService
{
	private static final Logger LOG = LoggerFactory.getLogger(ClientContactServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private BezwaarService bezwaarService;

	@Autowired
	private ColonUitnodigingService colonUitnodigingsService;

	@Autowired
	private IFobtService ifobtService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private ColonHuisartsService colonHuisartsService;

	@Autowired
	private CervixBaseScreeningrondeService cervixBaseScreeningrondeService;

	@Autowired
	private ColonScreeningsrondeService colonScreeningsrondeService;

	@Autowired
	private MammaBaseAfspraakService mammaBaseAfspraakService;

	@Autowired
	private MammaBaseDossierService mammaBaseDossierService;

	@Autowired
	private MammaBaseUitstelService mammaBaseUitstelService;

	@Autowired
	private MammaBaseFactory mammaBaseFactory;

	@Autowired
	private MammaHuisartsService mammaHuisartsService;

	@Autowired
	private MammaBaseOnderzoekService mammaBaseOnderzoekService;

	@Autowired
	private BaseOverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private CervixBaseUitnodigingService cervixUitnodigingService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaBaseBeoordelingService beoordelingService;

	@Autowired
	private BriefHerdrukkenService briefHerdrukkenService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private MammaAfmeldService mammaAfmeldService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED, rollbackFor = { HibernateJdbcException.class, MammaTijdNietBeschikbaarException.class, GenericJDBCException.class })
	public void saveClientContact(ClientContact contact, Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, Account account)
	{
		@SuppressWarnings("unchecked")
		boolean isInstellingGebruiker = Hibernate.getClass(account).isAssignableFrom(InstellingGebruiker.class);
		if (isInstellingGebruiker)
		{
			contact.setInstellingGebruiker((InstellingGebruiker) account);
		}
		List<ClientContactActie> actiesToDelete = new ArrayList<>();
		Client client = contact.getClient();
		mammaDoelgroepGewijzigd(contact, account);
		contact = verwerkEerstHuisartsWijzigenActie(contact, actiesToDelete, account, extraOpslaanObjecten, isInstellingGebruiker);
		for (ClientContactActie actie : contact.getActies())
		{
			Map<ExtraOpslaanKey, Object> extraOpslaanParams = extraOpslaanObjecten.get(actie.getType());
			switch (actie.getType())
			{
			case COLON_AANVRAGEN_NIEUWE_IFOBT:
				vraagNieuweIfobtAan(client, account);
				break;
			case OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
				clientService.vraagGbaGegevensOpnieuwAan(client, account, actie.getOpnieuwAanvragenClientgegevensReden());
				break;
			case TIJDELIJK_ADRES:
				tijdelijkAdres(account, client, actie, extraOpslaanParams);
				break;
			case COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN:
				actie = afspraakWijzigenAfzeggen(account, actiesToDelete, actie, extraOpslaanParams);
				break;
			case COLON_NIEUWE_AFSPRAAK_AANMAKEN:
				actie = nieuweAfspraakMaken(account, client, actiesToDelete, actie, extraOpslaanParams);
				break;
			case BEZWAAR:
				actie = bezwaarViaClientContact(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case INZAGE_PERSOONSGEGEVENS:
				overdrachtPersoonsgegevensService.maakOverdrachtVerzoek(client);
				break;
			case COLON_AFMELDEN:
			case CERVIX_AFMELDEN:
			case MAMMA_AFMELDEN:
				actie = afmelden(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case COLON_HERAANMELDEN:
			case CERVIX_HERAANMELDEN:
			case MAMMA_HERAANMELDEN:
				actie = heraanmelden(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_UITSTEL:
				actie = cervixUitstel(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_ZAS_AANVRAGEN:
				actie = zasAanvragen(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_HERDRUK:
				actie = cervixHerdruk(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_FRISSE_START:
				actie = cervixFrisseStart(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case COLON_HUISARTS_WIJZIGEN:
			case MAMMA_HUISARTS_WIJZIGEN:
				actie = null;
				break;
			case MAMMA_AFSPRAAK_MAKEN_FORCEREN:
			case MAMMA_RONDE_FORCEREN:
			case MAMMA_AFSPRAAK_MAKEN:
			case MAMMA_AFSPRAAK_WIJZIGEN:
				actie = mammaAfspraakMakenWijzigen(actie, client, extraOpslaanParams, account,
					ClientContactActieType.MAMMA_AFSPRAAK_MAKEN.equals(actie.getType()) || ClientContactActieType.MAMMA_RONDE_FORCEREN.equals(actie.getType()),
					ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN.equals(actie.getType()));
				break;
			case MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS:
				actie = mammaMinderValideOnderzoekZiekenhuis(actie, client, account);
				break;
			case MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS:
				actie = mammaMinderValideNietMeerOnderzoekZiekenhuis(actie, client, account);
				break;
			case MAMMA_INFOBRIEF_PROTHESEN:
				actie = mammaInfobriefProthesen(actie, client, account);
				break;
			case MAMMA_DOELGROEP_WIJZIGEN:
				break; 
			case COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
				actie = colonAanvraagVerwijderenUitslagBrief(actie, client);
				break;
			case CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
				actie = cervixAanvraagVerwijderenUitslagBrief(actie, client);
				break;
			case MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK:
				mammaClientWilGeenVervolgOnderzoek(client);
				break;
			case MAMMA_VERZOEK_CLIENT_CONTACT:
				mammaStuurVerzoekOmContact(client);
				break;
			case MAMMA_HERBEOORDELEN:
				mammaAnnuleerBeoordeling(client, contact.getInstellingGebruiker());
				break;
			default:
				LOG.warn("Actie type niet verwerkt/opgeslagen. Type: " + actie.getType());
				break;
			}
			if (actie != null && isInstellingGebruiker)
			{
				hibernateService.saveOrUpdate(actie);
			}
		}
		for (ClientContactActie actieToDelete : actiesToDelete)
		{
			contact.getActies().remove(actieToDelete);
		}
		if (isInstellingGebruiker)
		{
			String uitgevoerdeActies = contact.getActies().stream().map(a -> a.getType().name()).collect(Collectors.joining(", "));
			logService.logGebeurtenis(LogGebeurtenis.CLIENTCONTACT_REGISTREREN, account, client, "Aangemaakt met vervolgstap(pen): " + uitgevoerdeActies);
			hibernateService.saveOrUpdate(contact);
		}
	}

	private void mammaAnnuleerBeoordeling(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		MammaBeoordeling laatsteBeoordeling = MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(client);
		beoordelingService.valideerEnHerbeoordeelBeoordeling(laatsteBeoordeling, ingelogdeGebruiker);
	}

	private void mammaStuurVerzoekOmContact(Client client)
	{
		briefService.maakMammaBrief(client.getMammaDossier().getLaatsteScreeningRonde(), BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT);
	}

	private void mammaClientWilGeenVervolgOnderzoek(Client client)
	{
		MammaOnderzoek onderzoek = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteOnderzoek();
		mammaBaseOnderzoekService.vervolgOnderbrokenOnderzoeken(onderzoek);
	}

	private ClientContactActie cervixAanvraagVerwijderenUitslagBrief(ClientContactActie actie, Client client)
	{
		baseBriefService.maakCervixBrief(client.getCervixDossier().getLaatsteScreeningRonde(), BriefType.CERVIX_VERWIJDER_UITSLAG);
		return actie;
	}

	private ClientContactActie colonAanvraagVerwijderenUitslagBrief(ClientContactActie actie, Client client)
	{
		baseBriefService.maakColonBrief(client.getColonDossier().getLaatsteScreeningRonde(), BriefType.COLON_VERWIJDER_UITSLAG);
		return actie;
	}

	private void mammaDoelgroepGewijzigd(ClientContact contact, Account account)
	{
		Client client = contact.getClient();
		Optional<ClientContactActie> mammaDoelgroepVastleggenActie = contact.getActies().stream().filter(c -> ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (mammaDoelgroepVastleggenActie.isPresent())
		{
			MammaDossier mammaDossier = client.getMammaDossier();
			MammaDoelgroep doelgroep = mammaDossier.getDoelgroep();
			if (!doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD))
			{
				mammaDossier.setDubbeleTijdReden(null);
			}
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_DOELGROEP_GEWIJZIGD, account, client, doelgroep.toString(), Bevolkingsonderzoek.MAMMA);
			baseKansberekeningService.dossierEventHerzien(mammaDossier);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED, rollbackFor = { HibernateJdbcException.class, MammaTijdNietBeschikbaarException.class, GenericJDBCException.class })
	public ClientContactActie mammaAfspraakMakenWijzigen(ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams, Account account,
		boolean isNieuweAfspraak, boolean isGeforceerdeAfspraak)
	{
		boolean briefAanmaken = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_BRIEF_AANMAKEN));
		MammaUitstel uitstel = (MammaUitstel) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_UITSTEL);
		baseKansberekeningService.resetPreferences();
		if (uitstel != null)
		{
			mammaBaseUitstelService.saveUitstel(uitstel, briefAanmaken, account);
		}
		MammaAfspraak afspraak = (MammaAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
		if (afspraak != null)
		{
			if (isGeforceerdeAfspraak)
			{
				MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
				heropenRonde(laatsteScreeningRonde);
				MammaBeoordeling laatsteBeoordeling = laatsteScreeningRonde.getLaatsteOnderzoek().getLaatsteBeoordeling();
				if (laatsteBeoordeling != null)
				{
					beoordelingService.annuleerBeoordeling(laatsteBeoordeling);
				}
			}
			MammaUitnodiging laatsteUitnodiging = afspraak.getUitnodiging().getScreeningRonde().getLaatsteUitnodiging();
			if (laatsteUitnodiging == null && afspraak.getUitnodiging().getScreeningRonde().getIsGeforceerd() && isNieuweAfspraak)
			{
				MammaUitnodiging tempUitnodiging = afspraak.getUitnodiging();
				MammaScreeningRonde screeningRonde = mammaBaseFactory.maakRonde(tempUitnodiging.getScreeningRonde().getDossier(),
					tempUitnodiging.getScreeningRonde().getStandplaatsRonde(), true);
				MammaUitnodiging uitnodiging = mammaBaseFactory.maakUitnodiging(screeningRonde, afspraak.getStandplaatsPeriode().getStandplaatsRonde(),
					BriefType.MAMMA_AFSPRAAK_UITNODIGING);
				boolean briefTegenhouden = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN));
				uitnodiging.getBrief().setTegenhouden(briefTegenhouden);

				afspraak.setUitnodiging(uitnodiging);
			}

			if (!isTijdBeschikbaar(afspraak, (IMammaAfspraakWijzigenFilter) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_AFSPRAAK_FILTER)))
			{
				throw new MammaTijdNietBeschikbaarException();
			}

			beoordelingService
				.zoekOpgeschorteBeoordelingInRonde(client.getMammaDossier().getLaatsteScreeningRonde(),
					MammaBeoordelingOpschortenReden.AANVULLENDE_BEELDEN_NODIG_SE, MammaBeoordelingOpschortenReden.PRIORS_VAN_BUITEN_BVO)
				.ifPresent(beoordeling -> {
					beoordelingService.setStatus(beoordeling, MammaBeoordelingStatus.OPGESCHORT_MET_AFSPRAAK);
					hibernateService.saveOrUpdate(beoordeling);
				});

			boolean vorigeAfspraakVerzetten = actie.getType().equals(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN);

			mammaBaseAfspraakService.maakAfspraak(afspraak.getUitnodiging().getScreeningRonde(), afspraak.getCapaciteitBlok(), afspraak.getVanaf(),
				afspraak.getStandplaatsPeriode(), afspraak.getVerzettenReden(), vorigeAfspraakVerzetten, true, false, true, true, account,
				isGeforceerdeAfspraak);

			valideerToekomstigeAfspraken(afspraak);

			if (briefAanmaken || MammaVerzettenReden.briefVerplicht(afspraak.getVerzettenReden()))
			{
				briefService.maakMammaBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET);
			}

			PlanningVerzetClientenDto verzetClientenDto = new PlanningVerzetClientenDto();
			verzetClientenDto.clientIdSet.add(client.getId());
			verzetClientenDto.verzetStandplaatsPeriodeId = afspraak.getStandplaatsPeriode().getId();
			baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);
		}

		return actie;
	}

	private void heropenRonde(ScreeningRonde screeningRonde)
	{
		if (ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
		{
			screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			screeningRonde.setStatusDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(screeningRonde);
		}
	}

	private void valideerToekomstigeAfspraken(MammaAfspraak afspraak)
	{
		AtomicBoolean heeftGeplandeAfspraak = new AtomicBoolean(false);
		afspraak.getUitnodiging().getScreeningRonde().getUitnodigingen().forEach(uitnodiging -> uitnodiging.getAfspraken().forEach(af -> {
			if (af.getStatus() == MammaAfspraakStatus.GEPLAND && af.getVanaf().compareTo(currentDateSupplier.getDate()) > 0)
			{
				if (heeftGeplandeAfspraak.get())
				{
					throw new IllegalStateException("Er zijn meerdere afspraken in de toekomst, dit is niet toegestaan");
				}
				else
				{
					heeftGeplandeAfspraak.set(true);
				}
			}
		}));
	}

	private boolean isTijdBeschikbaar(MammaAfspraak nieuweAfspraak, IMammaAfspraakWijzigenFilter afspraakWijzigenFilter)
	{
		MammaDossier dossier = nieuweAfspraak.getUitnodiging().getScreeningRonde().getDossier();
		MammaCapaciteitBlok capaciteitBlok = nieuweAfspraak.getCapaciteitBlok();
		Date nieuweVanaf = nieuweAfspraak.getVanaf();
		if (dossier.getTehuis() != null && !capaciteitBlok.getBlokType().equals(MammaCapaciteitBlokType.TEHUIS)
			|| dossier.getTehuis() == null && !capaciteitBlok.getBlokType().equals(MammaCapaciteitBlokType.REGULIER)
			|| DateUtil.compareBefore(nieuweVanaf, capaciteitBlok.getVanaf())
			|| !DateUtil.compareBefore(nieuweVanaf, capaciteitBlok.getTot()))
		{
			LOG.trace("isTijdBeschikbaar: Valt buiten blok/type");
			return false;
		}
		LOG.trace("isTijdBeschikbaar: nieuweVanaf " + nieuweVanaf + " " + capaciteitBlok.getId());

		if (capaciteitBlok.getAfspraken().stream().anyMatch(afspraak -> DateUtil.compareEquals(nieuweVanaf, afspraak.getVanaf())))
		{
			afspraakWijzigenFilter.setBuitenRegio(true);
			afspraakWijzigenFilter.setExtraOpties(true);
			List<MammaKandidaatAfspraakDto> kandidaatAfspraken = mammaBaseAfspraakService
				.getKandidaatAfspraken(nieuweAfspraak.getUitnodiging().getScreeningRonde().getDossier().getClient(), afspraakWijzigenFilter);

			if (LOG.isTraceEnabled())
			{
				StringBuilder afspraken = new StringBuilder();
				kandidaatAfspraken.forEach(a -> afspraken.append(a.getDatum() + " " + a.getTijd() + " " + a.getCapaciteitBlokId() + ", "));
				LOG.trace("isTijdBeschikbaar: Kandaten " + afspraken);
			}
			LocalTime tijd = DateUtil.toLocalTime(nieuweVanaf);
			LocalDate datum = DateUtil.toLocalDate(nieuweVanaf);

			return kandidaatAfspraken.stream().anyMatch(kandidaatAfspraakDto -> {
				return tijd.equals(kandidaatAfspraakDto.getTijd()) && datum.equals(kandidaatAfspraakDto.getDatum())
					&& capaciteitBlok.getId().equals(kandidaatAfspraakDto.getCapaciteitBlokId());
			});
		}
		else if (LOG.isTraceEnabled())
		{
			StringBuilder afspraken = new StringBuilder();
			capaciteitBlok.getAfspraken().forEach(a -> afspraken.append(a.getVanaf() + ", "));
			LOG.info("isTijdBeschikbaar: afspraken " + afspraken.toString());
		}
		return true;
	}

	private ClientContactActie mammaMinderValideNietMeerOnderzoekZiekenhuis(ClientContactActie actie, Client client, Account account)
	{
		MammaScreeningRonde laatsteRonde = client.getMammaDossier().getLaatsteScreeningRonde();
		MammaBrief laatsteBrief = laatsteRonde.getLaatsteBrief();
		laatsteRonde.setMinderValideOnderzoekZiekenhuis(false);
		if (laatsteBrief != null && BriefType.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS.equals(laatsteBrief.getBriefType()))
		{
			if (!laatsteBrief.isGegenereerd())
			{
				laatsteBrief.setTegenhouden(true);
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, account, client,
					BriefUtil.getBriefTypeNaam(laatsteBrief) + ", wordt tegengehouden.", laatsteBrief.getBriefType().getOnderzoeken());
			}
		}
		hibernateService.saveOrUpdateAll(laatsteBrief, laatsteRonde);
		return actie;
	}

	private ClientContactActie mammaMinderValideOnderzoekZiekenhuis(ClientContactActie actie, Client client, Account account)
	{
		MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();

		MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			mammaBaseAfspraakService.afspraakAnnuleren(laatsteAfspraak, MammaAfspraakStatus.GEANNULEERD_VIA_INFOLIJN, null);
		}

		if (laatsteScreeningRonde.getLaatsteUitstel() != null)
		{
			mammaBaseUitstelService.uitstelAfzeggen(laatsteScreeningRonde.getLaatsteUitstel(), MammaUitstelGeannuleerdReden.MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
				currentDateSupplier.getDate());
		}

		laatsteScreeningRonde.setMinderValideOnderzoekZiekenhuis(true);

		baseBriefService.maakMammaBrief(laatsteScreeningRonde, BriefType.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS);
		hibernateService.saveOrUpdate(laatsteScreeningRonde);

		return actie;
	}

	private ClientContactActie mammaInfobriefProthesen(ClientContactActie actie, Client client, Account account)
	{
		baseBriefService.maakMammaBrief(client.getMammaDossier().getLaatsteScreeningRonde(), BriefType.MAMMA_INFOBRIEF_PROTHESEN);

		return actie;
	}

	private ClientContact verwerkEerstHuisartsWijzigenActie(ClientContact contact, List<ClientContactActie> actiesToDelete, Account account,
		Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, boolean isInstellingGebruiker)
	{
		verwerkEerstColonHuisartsWijzigenActie(contact, actiesToDelete, account, extraOpslaanObjecten, isInstellingGebruiker);
		verwerkEerstMammaHuisartsWijzigenActie(contact, actiesToDelete, account, extraOpslaanObjecten, isInstellingGebruiker);
		return contact;
	}

	private void verwerkEerstMammaHuisartsWijzigenActie(ClientContact contact, List<ClientContactActie> actiesToDelete, Account account,
		Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, boolean isInstellingGebruiker)
	{
		Optional<ClientContactActie> huisartsWijzigenActie = contact.getActies().stream().filter(c -> ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (huisartsWijzigenActie.isPresent())
		{
			ClientContactActie actie = huisartsWijzigenActie.get();
			Map<ExtraOpslaanKey, Object> extraOpslaanParams = extraOpslaanObjecten.get(ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN);
			actie = wijzigMammaHuisarts(account, actie, contact.getClient(), actiesToDelete, extraOpslaanParams);

			if (actie != null && isInstellingGebruiker)
			{
				hibernateService.saveOrUpdate(actie);
			}
		}
	}

	private void verwerkEerstColonHuisartsWijzigenActie(ClientContact contact, List<ClientContactActie> actiesToDelete, Account account,
		Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, boolean isInstellingGebruiker)
	{
		Optional<ClientContactActie> huisartsWijzigenActie = contact.getActies().stream().filter(c -> ClientContactActieType.COLON_HUISARTS_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (huisartsWijzigenActie.isPresent())
		{
			ClientContactActie actie = huisartsWijzigenActie.get();
			Map<ExtraOpslaanKey, Object> extraOpslaanParams = extraOpslaanObjecten.get(ClientContactActieType.COLON_HUISARTS_WIJZIGEN);
			boolean isAfspraakActieAanwezig = contact.getActies().stream().filter(
				c -> (ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN.equals(c.getType()) || ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN.equals(c.getType())))
				.findFirst().isPresent();
			actie = wijzigColonHuisarts(account, actie, contact.getClient(), actiesToDelete, extraOpslaanParams, isAfspraakActieAanwezig);

			if (actie != null && isInstellingGebruiker)
			{
				hibernateService.saveOrUpdate(actie);
			}
		}
	}

	private ClientContactActie wijzigColonHuisarts(Account account, ClientContactActie actie, Client client, List<ClientContactActie> actiesToDelete,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams, boolean isAfspraakActieAanwezig)
	{
		ColonScreeningRonde ronde = (ColonScreeningRonde) extraOpslaanParams.get(ExtraOpslaanKey.COLON_HUISARTS);
		Boolean huisartsBerichtenVerzenden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.COLON_HUISARTSBERICHTEN_VERZENDEN);

		boolean isGewijzigd = colonHuisartsService.koppelHuisarts(ronde.getColonHuisarts(), ronde, account);

		if (!isAfspraakActieAanwezig && Boolean.TRUE.equals(huisartsBerichtenVerzenden) && ronde.getColonHuisarts() != null)
		{
			afspraakService.verzendHuisartsBerichtOpnieuw(client, account);
		}
		else if (!isGewijzigd)
		{
			actiesToDelete.add(actie);
			actie = null;
		}
		return actie;
	}

	private ClientContactActie wijzigMammaHuisarts(Account account, ClientContactActie actie, Client client, List<ClientContactActie> actiesToDelete,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		MammaScreeningRonde ronde = (MammaScreeningRonde) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_HUISARTS);

		boolean isGewijzigd = mammaHuisartsService.koppelHuisarts(ronde.getHuisarts(), ronde, account);

		if (!isGewijzigd)
		{
			actiesToDelete.add(actie);
			actie = null;
		}
		return actie;
	}

	private ClientContactActie cervixHerdruk(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		CervixBrief cervixBrief = (CervixBrief) extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_HERDRUK_BRIEF);
		cervixBrief.setAangevraagdeHerdruk(true);
		briefHerdrukkenService.opnieuwAanmaken(cervixBrief, account);
		return actie;
	}

	private ClientContactActie cervixFrisseStart(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		CervixScreeningRonde laatsteRonde = client.getCervixDossier().getLaatsteScreeningRonde();

		if (laatsteRonde.getUitstel() != null)
		{
			cervixBaseScreeningrondeService.annuleerUitstel(laatsteRonde);
			LOG.info("Uitstel geannuleerd nav frisse start voor clientId: {}", client.getId());
		}
		factory.maakUitnodiging(laatsteRonde, BriefType.CERVIX_UITNODIGING);
		return actie;
	}

	private ClientContactActie zasAanvragen(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		boolean zasAangevraagdDoorClient = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.ZAS_DOOR_CLIENT_AANGEVRAAGD));
		boolean zasUitstelPeriodeNemen = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_UITSTEL));
		factory.maakZasUitnodiging(client, account, zasUitstelPeriodeNemen, zasAangevraagdDoorClient);
		return actie;
	}

	private ClientContactActie cervixUitstel(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		CervixUitstel uitstel = (CervixUitstel) extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_UITSTEL);
		if (uitstel != null)
		{
			cervixBaseScreeningrondeService.uitstelAanvragen(client, uitstel, account);
		}
		else
		{
			actie = null;
		}
		return actie;
	}

	private ClientContactActie tijdelijkAdres(Account account, Client client, ClientContactActie actie, Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		TijdelijkAdres tijdelijkAdres = (TijdelijkAdres) extraOpslaanParams.get(ExtraOpslaanKey.TIJDELIJK_ADRES);
		if (tijdelijkAdres != null)
		{
			saveTijdelijkAdres(account, client, tijdelijkAdres);
		}
		else
		{
			actie = null;
		}
		return actie;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveTijdelijkAdres(Account account, Client client, TijdelijkAdres tijdelijkAdres)
	{
		client.getPersoon().setTijdelijkAdres(tijdelijkAdres);
		tijdelijkAdres.setPostcodeCoordinaten(coordinatenDao.getCoordinaten(tijdelijkAdres));
		hibernateService.saveOrUpdateAll(tijdelijkAdres, client);
		logService.logGebeurtenis(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES, account, client);
	}

	private ClientContactActie nieuweAfspraakMaken(Account account, Client client, List<ClientContactActie> actiesToDelete, ClientContactActie actie,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		boolean kanNieuweAfspraakAanmaken = false;
		if (extraOpslaanParams != null && extraOpslaanParams.size() >= 1)
		{
			NieuweIntakeAfspraakMakenReden reden = (NieuweIntakeAfspraakMakenReden) extraOpslaanParams.get(ExtraOpslaanKey.REDEN_NIEUWE_AFSPRAAK);
			ColonIntakeAfspraak nieuweAfspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
			BriefType briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
			Boolean briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
			Boolean afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
			afspraakService.maakNieuweAfspraak(client, reden, nieuweAfspraak, !Boolean.FALSE.equals(briefTegenhouden), Boolean.TRUE.equals(afspraakUitRooster), briefType, account);

			kanNieuweAfspraakAanmaken = true;
		}
		if (!kanNieuweAfspraakAanmaken)
		{
			actiesToDelete.add(actie);
			actie = null;
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.NIEUWE_AFSPRAAK_AANGEMAAKT, account, client, Bevolkingsonderzoek.COLON);
		}
		return actie;
	}

	private ClientContactActie heraanmelden(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		Afmelding herAanTeMeldenAfmelding = (Afmelding) extraOpslaanParams.get(ExtraOpslaanKey.HERAANMELDING);
		if (herAanTeMeldenAfmelding == null)
		{
			actiesToDelete.add(actie);
			return null;
		}
		else
		{
			if (herAanTeMeldenAfmelding.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON && extraOpslaanParams != null && extraOpslaanParams.size() >= 1)
			{
				ColonIntakeAfspraak afspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
				Boolean afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
				BriefType briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
				Boolean briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
				boolean clientWilNieuweUitnodiging = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.NIEUWE_UITNODIGING));
				herAanTeMeldenAfmelding.setClientWilNieuweUitnodiging(clientWilNieuweUitnodiging);
				((ColonAfmelding) herAanTeMeldenAfmelding).setHeraanmeldingAfspraak(afspraak);
				((ColonAfmelding) herAanTeMeldenAfmelding).setHeraanmeldingAfspraakUitRooster(afspraakUitRooster);
				((ColonAfmelding) herAanTeMeldenAfmelding).setHeraanmeldingAfspraakBriefType(briefType);
				((ColonAfmelding) herAanTeMeldenAfmelding).setHeraanmeldingAfspraakBriefTegenhouden(briefTegenhouden);
			}
			baseAfmeldService.heraanmelden(herAanTeMeldenAfmelding, account);
			return actie;
		}
	}

	private ClientContactActie bezwaarViaClientContact(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams, Account account)
	{
		if (extraOpslaanParams != null && extraOpslaanParams.size() >= 1 && extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR) != null)
		{
			BezwaarMoment toegevoegdeBezwaar = (BezwaarMoment) extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR);
			if (ClientContactManier.AANVRAAG_FORMULIER.equals(toegevoegdeBezwaar.getManier()))
			{
				bezwaarService.bezwaarAanvragen(client, toegevoegdeBezwaar);
			}
			else
			{

				hibernateService.saveOrUpdate(toegevoegdeBezwaar);
				List<BezwaarGroupViewWrapper> groupWrappers = (List<BezwaarGroupViewWrapper>) extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR_WRAPPERS);
				bezwaarService.bezwaarAfronden(toegevoegdeBezwaar, account, groupWrappers);
			}

		}
		else
		{
			actiesToDelete.add(actie);
			actie = null;
		}

		return actie;
	}

	private ClientContactActie afmelden(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		Afmelding afmelding = (Afmelding) extraOpslaanParams.get(ExtraOpslaanKey.AFMELDING);
		if (afmelding == null)
		{
			actiesToDelete.add(actie);
			return null;
		}
		else
		{
			baseAfmeldService.afmelden(client, afmelding, account);
			logService.logGebeurtenis(LogGebeurtenis.AFMELDEN, account, client, "Type: " + afmelding.getType().name().toLowerCase(), afmelding.getBevolkingsonderzoek());
			return actie;
		}
	}

	private ClientContactActie afspraakWijzigenAfzeggen(Account account, List<ClientContactActie> actiesToDelete, ClientContactActie actie,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		if (extraOpslaanParams != null && extraOpslaanParams.size() >= 2)
		{
			ColonIntakeAfspraak afspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
			AfspraakStatus nieuweAfspraakStatus = (AfspraakStatus) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_STATUS);
			BriefType briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
			Boolean briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
			Boolean afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
			if (AfspraakStatus.VERPLAATST.equals(nieuweAfspraakStatus))
			{
				afspraakService.verplaatsAfspraak(afspraak, account, briefType, !Boolean.FALSE.equals(briefTegenhouden), Boolean.TRUE.equals(afspraakUitRooster));
			}
			else if (AfspraakStatus.GEANNULEERD_VIA_INFOLIJN.equals(nieuweAfspraakStatus))
			{
				afspraakService.annuleerAfspraak(afspraak, account, nieuweAfspraakStatus, !Boolean.FALSE.equals(briefTegenhouden));
			}
		}
		else
		{
			actiesToDelete.add(actie);
			actie = null;
		}
		return actie;
	}

	@Override
	public List<ClientContactActieType> getAvailableActies(Client client)
	{
		return getAvailableActies(client, false);
	}

	@Override
	public List<ClientContactActieType> getAvailableActies(Client client, boolean viaClientportaal)
	{
		List<ClientContactActieType> availableActies = new ArrayList<>();
		boolean clientOverleden = clientService.isClientOverleden(client);
		boolean clientInBuitenland = clientService.clientInBuitenland(client);

		if (clientOverleden)
		{
			return availableActies;
		}

		if (clientInBuitenland)
		{
			return List.of(ClientContactActieType.GEEN);
		}

		boolean toonVervangendeTekstMammaOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false);
		boolean toonVervangendeTekstCervixOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(),
				false);
		boolean toonVervangendeTekstColonOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false);

		boolean behoortTotDoelgroepColon = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.COLON);
		boolean behoortTotDoelgroepCervix = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.CERVIX);
		boolean behoortTotDoelgroepMamma = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.MAMMA);
		for (ClientContactActieType actieType : ClientContactActieType.values())
		{

			if (actieType.equals(ClientContactActieType.GEEN) && viaClientportaal)
			{
				continue;
			}
			if (Bevolkingsonderzoek.heeftAlleBevolkingsonderzoeken(actieType.getBevolkingsonderzoeken())
				&& availableGen(client, actieType, behoortTotDoelgroepCervix, behoortTotDoelgroepColon, behoortTotDoelgroepMamma))
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepColon && Bevolkingsonderzoek.alleenDarmkanker(actieType.getBevolkingsonderzoeken())
				&& availableColon(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstColonOpClientPortaal)
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepCervix && Bevolkingsonderzoek.alleenBaarmoederhalskanker(actieType.getBevolkingsonderzoeken())
				&& availableCervix(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstCervixOpClientPortaal)
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepMamma && Bevolkingsonderzoek.alleenBorstkanker(actieType.getBevolkingsonderzoeken())
				&& availableMamma(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstMammaOpClientPortaal)
			{
				availableActies.add(actieType);
			}
		}
		return availableActies;
	}

	@Override
	public boolean availableActiesBevatBenodigdeActie(Client client, ClientContactActieType benodigdeActie)
	{
		List<ClientContactActieType> beschikbareActies = getAvailableActies(client, true);
		return beschikbareActies.contains(benodigdeActie);
	}

	private boolean availableGen(Client client, ClientContactActieType actieType, boolean behoortTotDoelgroepCervix, boolean behoortTotDoelgroepColon,
		boolean behoortTotDoelgroepMamma)
	{
		switch (actieType)
		{
		case GEEN:
			return true;
		case OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
			return GbaStatus.INDICATIE_AANWEZIG.equals(client.getGbaStatus());
		case TIJDELIJK_ADRES:
		case INZAGE_PERSOONSGEGEVENS:
		case CERVIX_DEELNAME_BUITEN_BVO_BMHK:
			return true;
		case BEZWAAR:
			if (behoortTotDoelgroepCervix || behoortTotDoelgroepColon || behoortTotDoelgroepMamma)
			{
				List<BezwaarMoment> bezwaren = client.getBezwaarMomenten();
				Collections.sort(bezwaren, new BezwaarComparator());
				return bezwaren.isEmpty() || AanvraagBriefStatus.VERWERKT.equals(bezwaren.get(0).getStatus());
			}
		default:
			return false;
		}
	}

	private boolean availableColon(Client client, ClientContactActieType actieType, boolean viaClientportaal)
	{
		ColonDossier colonDossier = client.getColonDossier();

		boolean isDossierAangemeld = colonDossier.getAangemeld();
		ColonAfmelding laatsteDefinitieveAfmelding;
		laatsteDefinitieveAfmelding = colonDossier.getLaatsteAfmelding();
		boolean isLaatsteDefinitieveAfmelding = laatsteDefinitieveAfmelding != null;

		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		boolean heeftNietVerlopenLaatsteScreeningRonde = ColonScreeningRondeUtil.isLaatsteScreeningRondeNietVerlopen(laatsteScreeningRonde);
		boolean isLaatsteRondeGeldigEnAangemeld = ColonScreeningRondeUtil.isLaatsteScreeningRondGeldigEnAangemeld(laatsteScreeningRonde);

		boolean isLaatsteDefinitieveAfmeldingVerwerkt = isLaatsteDefinitieveAfmelding && AanvraagBriefStatus.VERWERKT.equals(laatsteDefinitieveAfmelding.getAfmeldingStatus());
		boolean isLaatsteDefinitieveAfmeldingHeraangemeld = isLaatsteDefinitieveAfmeldingVerwerkt
			&& AanvraagBriefStatus.VERWERKT.equals(laatsteDefinitieveAfmelding.getHeraanmeldStatus());

		boolean isLaatsteAfmeldingProefBevolking = isLaatsteDefinitieveAfmeldingVerwerkt
			&& ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(laatsteDefinitieveAfmelding.getReden());

		boolean magAfspaakWijzigenAfzeggen = false;
		for (Afspraak afspraak : client.getAfspraken())
		{
			if (magAfspaakWijzigenAfzeggen = afspraakService.magWijzigenAfzeggen(afspraak))
			{
				break;
			}
		}

		switch (actieType)
		{
		case COLON_HUISARTS_WIJZIGEN:
			return heeftNietVerlopenLaatsteScreeningRonde;
		case COLON_AANVRAGEN_NIEUWE_IFOBT:
			return magNieuweIfobtAanvragen(client);
		case COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN:
			return magAfspaakWijzigenAfzeggen;
		case COLON_NIEUWE_AFSPRAAK_AANMAKEN:
			return afspraakService.magNieuweAfspraakMaken(client);
		case COLON_AFMELDEN:
			return !getAvailableAfmeldoptiesColon(client, viaClientportaal).isEmpty();
		case COLON_HERAANMELDEN:
			return !isDossierAangemeld && isLaatsteDefinitieveAfmeldingVerwerkt && (!isLaatsteDefinitieveAfmeldingHeraangemeld || isLaatsteAfmeldingProefBevolking)
				|| isDossierAangemeld && heeftNietVerlopenLaatsteScreeningRonde && !isLaatsteRondeGeldigEnAangemeld;
		case COLON_OPEN_UITNODIGING:
			if (heeftNietVerlopenLaatsteScreeningRonde)
			{
				boolean isErEenOpenUitnodiging = laatsteScreeningRonde.getOpenUitnodiging() != null;
				boolean isDeOpenUitnodigingGemerged = isErEenOpenUitnodiging && laatsteScreeningRonde.getOpenUitnodiging().getUitnodigingsBrief() != null
					&& laatsteScreeningRonde.getOpenUitnodiging().getUitnodigingsBrief().getMergedBrieven() != null;
				boolean isErEenLaatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak() != null;
				boolean isDeLaatsteAfspraakGeannuleerd = isErEenLaatsteAfspraak && AfspraakStatus.isGeannuleerd(laatsteScreeningRonde.getLaatsteAfspraak().getStatus());
				boolean isColonDossierIsAfgemeldViaAfmelding = !colonDossier.getAangemeld();
				return isDeOpenUitnodigingGemerged && isLaatsteRondeGeldigEnAangemeld && !isColonDossierIsAfgemeldViaAfmelding
					&& (!isErEenLaatsteAfspraak || isDeLaatsteAfspraakGeannuleerd);
			}
			return false;
		case COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
			if (heeftNietVerlopenLaatsteScreeningRonde)
			{
				return laatsteScreeningRonde.getIfobtTesten().stream()
					.anyMatch(test -> !IFOBTTestStatus.VERWIJDERD.equals(test.getStatus()) && test.getUitslag() != null);
			}
			return false;
		default:
			return false;
		}

	}

	private boolean availableCervix(Client client, ClientContactActieType actieType, boolean viaClientportaal)
	{
		CervixDossier dossier = client.getCervixDossier();

		boolean definitiefAfmelden = true;
		boolean definitiefHeraanmelden = false;
		boolean eenmaligAfmelden = false;
		boolean eenmaligHeraanmelden = false;
		boolean herdruk = false;
		boolean uitstel = false;
		boolean zasAanvragen = false;
		boolean frisseStart = false;

		CervixAfmelding laatsteDefinitieveAfmelding = dossier.getLaatsteAfmelding();
		definitiefAfmelden = dossier.getStatus() == DossierStatus.ACTIEF
			&& (laatsteDefinitieveAfmelding == null || laatsteDefinitieveAfmelding.getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT || viaClientportaal);
		definitiefHeraanmelden = laatsteDefinitieveAfmelding != null
			&& laatsteDefinitieveAfmelding.getAfmeldingStatus() == AanvraagBriefStatus.VERWERKT
			&& laatsteDefinitieveAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT
			&& client.getPersoon().getGeslacht() == Geslacht.VROUW;

		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null && dossier.getVolgendeRondeVanaf().after(currentDateSupplier.getDate()))
		{
			eenmaligHeraanmelden = !ronde.getAangemeld() && dossier.getAangemeld();

			if (ronde.getStatus() == ScreeningRondeStatus.LOPEND)
			{
				eenmaligAfmelden = ronde.getStatus() == ScreeningRondeStatus.LOPEND;
				uitstel = true;

				CervixUitnodiging laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
				if (laatsteUitnodiging != null)
				{
					herdruk = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(laatsteUitnodiging.getBrief())
						&& cervixMagNieuweUitnodigingAanvragen(dossier);
				}
				else
				{
					if (cervixBaseScreeningrondeService.kanFrisseStartMaken(ronde))
					{
						frisseStart = true;
					}
				}
				if (ronde.getMonsterHpvUitslag() == null && !ronde.getUitnodigingen().isEmpty())
				{
					CervixUitnodiging laatsteZasUitnodiging = ronde.getLaatsteZasUitnodiging();
					zasAanvragen = laatsteZasUitnodiging == null || laatsteZasUitnodiging.getGeannuleerdDatum() != null || laatsteZasUitnodiging.getMonster() != null;
				}
			}
		}

		switch (actieType)
		{
		case CERVIX_AFMELDEN:
			return definitiefAfmelden || eenmaligAfmelden;
		case CERVIX_HERAANMELDEN:
			return definitiefHeraanmelden || eenmaligHeraanmelden;
		case CERVIX_UITSTEL:
			return uitstel;
		case CERVIX_ZAS_AANVRAGEN:
			return zasAanvragen;
		case CERVIX_HERDRUK:
			return herdruk;
		case CERVIX_FRISSE_START:
			return frisseStart;
		case CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
			if (dossier != null && dossier.getLaatsteScreeningRonde() != null)
			{
				boolean laatsteMonsterKanUitslagVerwijderdWorden = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
					&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getMonster() != null
					&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getMonster().equals(cervixUitnodigingService.getUitnodigingMagVerwijderdWorden(
						dossier.getLaatsteScreeningRonde()));
				boolean laatsteZasKanVerwijderdWorden = dossier.getLaatsteScreeningRonde().getLaatsteZasUitnodiging() != null
					&& dossier.getLaatsteScreeningRonde().getLaatsteZasUitnodiging().getMonster() != null
					&& dossier.getLaatsteScreeningRonde().getLaatsteZasUitnodiging().getMonster().equals(cervixUitnodigingService.getUitnodigingMagVerwijderdWorden(
						dossier.getLaatsteScreeningRonde()));
				return laatsteMonsterKanUitslagVerwijderdWorden || laatsteZasKanVerwijderdWorden;
			}
			return false;
		default:
			return false;
		}
	}

	private boolean availableMamma(Client client, ClientContactActieType actieType, boolean viaClientportaal)
	{
		boolean afspraakMaken;
		boolean verzetten = false;
		boolean minderValideOnderzoekZiekenhuis = false;
		boolean minderValideNietMeerOnderzoekZiekenhuis = false;
		boolean magClientVervolgOnderzoekAfwijzen = false;
		boolean magClientVerzoekVoorContactOpsturen = false;
		boolean infoBriefProthesenKlaarzetten = false;
		boolean magDoelgroepWijzigen = false;
		boolean magBeoordelingAnnuleren = false;

		MammaDossier dossier = client.getMammaDossier();
		MammaScreeningRonde laatsteRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteRonde != null)
		{
			MammaUitnodiging laatsteUitnodiging = laatsteRonde.getLaatsteUitnodiging();
			MammaAfspraak laatsteAfspraak = laatsteUitnodiging != null ? laatsteUitnodiging.getLaatsteAfspraak() : null;

			if (laatsteRonde.getStatus() == ScreeningRondeStatus.LOPEND)
			{
				minderValideOnderzoekZiekenhuis = dossier.getDoelgroep() == MammaDoelgroep.MINDER_VALIDE && laatsteUitnodiging != null
					&& !laatsteRonde.getMinderValideOnderzoekZiekenhuis() && (laatsteAfspraak == null || laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND
						|| MammaAfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus()));
				minderValideNietMeerOnderzoekZiekenhuis = laatsteRonde.getMinderValideOnderzoekZiekenhuis();
				MammaBrief vorigeInfobriefProthesen = laatsteRonde.getBrieven().stream().filter(brief -> BriefType.MAMMA_INFOBRIEF_PROTHESEN.equals(brief.getBriefType()))
					.max(Comparator.comparing(Brief::getCreatieDatum))
					.orElse(null);

				infoBriefProthesenKlaarzetten = vorigeInfobriefProthesen == null
					|| vorigeInfobriefProthesen.getMergedBrieven() != null && vorigeInfobriefProthesen.getMergedBrieven().getGeprint();
			}
			magDoelgroepWijzigen = !minderValideNietMeerOnderzoekZiekenhuis;
			if (dossier.getStatus() != DossierStatus.INACTIEF)
			{
				MammaOnderzoek laatsteOnderzoek = laatsteRonde.getLaatsteOnderzoek();
				MammaBeoordeling laatsteBeoordeling = laatsteOnderzoek != null ? MammaScreeningRondeUtil.getLaatsteBeoordeling(laatsteOnderzoek) : null;
				if (laatsteBeoordeling != null)
				{
					magBeoordelingAnnuleren = MammaBeoordelingStatus.isAnnulerenMogelijk(laatsteBeoordeling.getStatus());
				}
				if (laatsteOnderzoek != null && laatsteOnderzoek.getStatus() == MammaOnderzoekStatus.ONDERBROKEN && laatsteOnderzoek.isDoorgevoerd())
				{
					magClientVervolgOnderzoekAfwijzen = true;
					if (laatsteRonde.getLaatsteBrief() == null || laatsteRonde.getLaatsteBrief().getBriefType() != BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT)
					{
						magClientVerzoekVoorContactOpsturen = true;
					}
				}
			}
		}

		afspraakMaken = mammaBaseDossierService.isAfspraakMakenMogelijk(dossier, viaClientportaal, false);
		if (!afspraakMaken)
		{
			verzetten = mammaBaseDossierService.isVerzettenMogelijk(dossier);
		}

		switch (actieType)
		{
		case MAMMA_AFMELDEN:
			return mammaAfmeldService.magAfmelden(client, viaClientportaal);
		case MAMMA_HERAANMELDEN:
			return mammaAfmeldService.magHeraanmelden(client);
		case MAMMA_RONDE_FORCEREN:
			return mammaBaseDossierService.isRondeForcerenMogelijk(dossier);
		case MAMMA_AFSPRAAK_MAKEN_FORCEREN:
			return mammaBaseDossierService.isAfspraakForcerenMogelijk(dossier);
		case MAMMA_AFSPRAAK_MAKEN:
			return afspraakMaken;
		case MAMMA_AFSPRAAK_WIJZIGEN:
			return verzetten;
		case MAMMA_DOELGROEP_WIJZIGEN:
			return magDoelgroepWijzigen;
		case MAMMA_HUISARTS_WIJZIGEN:
			return laatsteRonde != null;
		case MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS:
			return minderValideOnderzoekZiekenhuis;
		case MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS:
			return minderValideNietMeerOnderzoekZiekenhuis;
		case MAMMA_INFOBRIEF_PROTHESEN:
			return infoBriefProthesenKlaarzetten;
		case MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK:
			return magClientVervolgOnderzoekAfwijzen;
		case MAMMA_VERZOEK_CLIENT_CONTACT:
			return magClientVerzoekVoorContactOpsturen;
		case MAMMA_HERBEOORDELEN:
			return magBeoordelingAnnuleren;
		default:
			return false;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public NieuweIfobtResultaat vraagNieuweIfobtAan(Client client, Account account)
	{
		ColonScreeningRonde laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();

		ColonUitnodiging laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null)
		{
			IFOBTTest test = laatsteUitnodiging.getGekoppeldeTest();
			if (test != null && !IFOBTTestStatus.VERWIJDERD.equals(test.getStatus()) && !IFOBTTestStatus.VERLOREN.equals(test.getStatus()))
			{
				ifobtService.markeerBuisAlsVerloren(laatsteUitnodiging);
			}
			ColonUitnodiging nieuweUitnodiging = colonUitnodigingsService.cloneUitnodiging(laatsteUitnodiging, true);
			if (nieuweUitnodiging != null)
			{
				nieuweUitnodiging.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U4);
				hibernateService.saveOrUpdate(nieuweUitnodiging);
			}
		}
		else
		{
			colonScreeningsrondeService.createNieuweUitnodiging(laatsteScreeningRonde, ColonUitnodigingCategorie.U4);
		}
		NieuweIfobtResultaat resultaat = NieuweIfobtResultaat.AANGEVRAAGD;

		NieuweIFobtAanvraagLogEvent logEvent = new NieuweIFobtAanvraagLogEvent();
		logEvent.setClient(client);
		logEvent.setResultaat(resultaat);
		logService.logGebeurtenis(LogGebeurtenis.NIEUWE_IFOBT_AANGEVRAAGD, logEvent, account, client, Bevolkingsonderzoek.COLON);

		return resultaat;
	}

	private boolean magNieuweIfobtAanvragen(Client client, boolean isHeraanmelding)
	{
		boolean resultaat = true;
		ColonDossier colonDossier = client.getColonDossier();

		if (colonDossier.getLaatsteScreeningRonde() == null)
		{
			return false;
		}
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		ColonAfmelding eenmaligeAfmelding = laatsteScreeningRonde.getLaatsteAfmelding();
		ColonAfmelding definitiefAfmelding = colonDossier.getLaatsteAfmelding();

		boolean magNieuweUitnodigingAanvragen = eenmaligeAfmelding != null && Boolean.TRUE.equals(eenmaligeAfmelding.getRondeGesloten())
			&& Boolean.FALSE.equals(eenmaligeAfmelding.getRondeHeropend())
			|| definitiefAfmelding != null && Boolean.TRUE.equals(definitiefAfmelding.getRondeGesloten())
				&& Boolean.FALSE.equals(definitiefAfmelding.getRondeHeropend());

		if (laatsteScreeningRonde.getStatus() != ScreeningRondeStatus.LOPEND && !isHeraanmelding || !magNieuweUitnodigingAanvragen && isHeraanmelding)
		{
			resultaat = false;
		}
		else if (ColonScreeningRondeUtil.heeftUitslagBrief(laatsteScreeningRonde))
		{
			resultaat = false;
		}
		else if (laatsteScreeningRonde.getOpenUitnodiging() != null)
		{
			resultaat = false;
		}
		else if (colonScreeningsrondeService.heeftMaxAantalFitAanvragenBereikt(laatsteScreeningRonde))
		{
			resultaat = false;
		}
		else if (isSpreidingsperiodeVerstreken(laatsteScreeningRonde))
		{
			resultaat = false;
		}
		else if (laatsteScreeningRonde.getLaatsteUitnodiging() != null && !laatsteScreeningRonde.getLaatsteUitnodiging().isVerstuurd())
		{
			resultaat = false;
		}
		return resultaat;
	}

	@Override
	public boolean magNieuweIfobtAanvragen(Client client)
	{
		return magNieuweIfobtAanvragen(client, false);
	}

	@Override
	public List<AfmeldingType> getAvailableAfmeldoptiesColon(Client client, boolean viaPortaalGevraagd)
	{
		List<AfmeldingType> afmeldTypes = new ArrayList<>();
		ColonDossier colonDossier = client.getColonDossier();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		boolean heeftNietVerlopenLaatsteScreeningRonde = laatsteScreeningRonde != null && !(ScreeningRondeStatus.AFGEROND.equals(laatsteScreeningRonde.getStatus())
			&& Constants.RONDE_AFROND_REDEN_BUITEN_DOELGROEP.equals(laatsteScreeningRonde.getAfgerondReden()));

		if (heeftNietVerlopenLaatsteScreeningRonde && Boolean.TRUE.equals(laatsteScreeningRonde.getAangemeld()))
		{
			if (ScreeningRondeStatus.AFGEROND.equals(laatsteScreeningRonde.getStatus()))
			{
				IFOBTTest ifobtTest = laatsteScreeningRonde.getLaatsteIFOBTTest();
				if (IFOBTTestUtil.isGunstig(ifobtTest))
				{
					afmeldTypes.add(AfmeldingType.EENMALIG);
				}
			}
			else
			{
				ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
				boolean geenColoscopie = true;
				if (laatsteAfspraak != null)
				{
					ColonConclusie conclusie = laatsteAfspraak.getConclusie();
					geenColoscopie = conclusie == null || !ColonConclusieType.COLOSCOPIE.equals(conclusie.getType());
				}
				if (geenColoscopie)
				{
					afmeldTypes.add(AfmeldingType.EENMALIG);
				}
			}
		}
		ColonAfmelding laatsteDefinitieveAfmelding = colonDossier.getLaatsteAfmelding();
		boolean isLaatsteAfmelding = laatsteDefinitieveAfmelding != null;
		boolean islaatsteAfmeldingBrief = isLaatsteAfmelding && AanvraagBriefStatus.BRIEF == laatsteDefinitieveAfmelding.getAfmeldingStatus();
		boolean islaatsteAfmeldingVerwerkt = isLaatsteAfmelding && AanvraagBriefStatus.VERWERKT == laatsteDefinitieveAfmelding.getAfmeldingStatus();

		if (Boolean.TRUE.equals(colonDossier.getAangemeld()) && (laatsteScreeningRonde == null || heeftNietVerlopenLaatsteScreeningRonde)
			&& (laatsteDefinitieveAfmelding == null || isLaatsteAfmelding && (laatsteDefinitieveAfmelding.getHeraanmeldStatus() == null && !islaatsteAfmeldingVerwerkt
				&& (viaPortaalGevraagd || !islaatsteAfmeldingBrief)
				|| AanvraagBriefStatus.VERWERKT == laatsteDefinitieveAfmelding.getHeraanmeldStatus())))
		{
			afmeldTypes.add(AfmeldingType.DEFINITIEF);
		}
		return afmeldTypes;
	}

	@Override
	public List<AfmeldingType> getAvailableAfmeldoptiesCervix(Client client, boolean viaPortaalGevraagd)
	{
		List<AfmeldingType> afmeldingTypes = new ArrayList<>();
		if (cervixIsEenmaligAfmeldenMogelijk(client))
		{
			afmeldingTypes.add(AfmeldingType.EENMALIG);
		}
		if (cervixIsDefinitiefAfmeldenMogelijk(client, viaPortaalGevraagd))
		{
			afmeldingTypes.add(AfmeldingType.DEFINITIEF);
		}
		return afmeldingTypes;
	}

	private boolean cervixIsDefinitiefAfmeldenMogelijk(Client client, boolean viaPortaalGevraagd)
	{
		CervixDossier dossier = client.getCervixDossier();
		return dossier.getAangemeld()
			&& (dossier.getLaatsteAfmelding() == null || dossier.getLaatsteAfmelding().getAfmeldingStatus() != AanvraagBriefStatus.BRIEF || viaPortaalGevraagd);
	}

	private boolean cervixIsEenmaligAfmeldenMogelijk(Client client)
	{
		CervixDossier dossier = client.getCervixDossier();
		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null && ronde.getAangemeld() && ronde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			return true;
		}
		return false;
	}

	@Override
	public boolean magNieuweUitnodigingAanvragen(Dossier dossier, boolean isHeraanmelding)
	{
		switch (dossier.getBevolkingsonderzoek())
		{
		case COLON:
			return magNieuweIfobtAanvragen(dossier.getClient(), isHeraanmelding);
		case CERVIX:
			return cervixMagNieuweUitnodigingAanvragen((CervixDossier) dossier);
		case MAMMA:
			return false; 
		}
		return false;
	}

	private boolean isSpreidingsperiodeVerstreken(ColonScreeningRonde ronde)
	{
		Integer uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		return DateUtil.compareBefore(ronde.getCreatieDatum(), DateUtil.toUtilDate(vandaag.minusDays(uitnodigingsinterval)));
	}

	@Override
	public boolean defaultNieuweUitnodigingAanvragen(Dossier dossier)
	{
		boolean nieuweUitnodiging = false;
		ScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		switch (dossier.getBevolkingsonderzoek())
		{
		case COLON:
			if (dossier.getScreeningRondes().size() == 1 && laatsteScreeningRonde != null)
			{
				Uitnodiging laatsteColonUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();
				if (laatsteScreeningRonde.getUitnodigingen().size() == 1 && laatsteColonUitnodiging != null)
				{
					nieuweUitnodiging = laatsteColonUitnodiging.getUitnodigingsDatum().after(currentDateSupplier.getDate());
				}
			}
			break;
		case CERVIX:
			nieuweUitnodiging = cervixMagNieuweUitnodigingAanvragen((CervixDossier) dossier);
			break;
		case MAMMA:
			break;
		}
		return nieuweUitnodiging;
	}

	private boolean cervixMagNieuweUitnodigingAanvragen(CervixDossier dossier)
	{
		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null)
		{
			if (ronde.getUitstrijkjeVervolgonderzoekUitslag() == null)
			{
				if (ronde.getUitnodigingVervolgonderzoek() != null)
				{
					return true;
				}

				if (ronde.getUitstrijkjeCytologieUitslag() == null)
				{
					CervixMonster monsterHpvUitslagHpvUitslag = ronde.getMonsterHpvUitslag();
					if (monsterHpvUitslagHpvUitslag != null)
					{
						if (monsterHpvUitslagHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF)
						{
							return true;
						}
					}
					else
					{
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public boolean heeftOpenIntakeAfspraak(Client client)
	{
		ColonIntakeAfspraak laatsteAfspraak = null;
		if (client != null && client.getColonDossier().getLaatsteScreeningRonde() != null)
		{
			laatsteAfspraak = client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfspraak();
		}
		if (laatsteAfspraak != null)
		{
			ColonConclusieType colonConclusieType = null;

			ColonConclusie conclusie = laatsteAfspraak.getConclusie();
			if (conclusie != null)
			{
				colonConclusieType = conclusie.getType();
			}

			AfspraakStatus status = laatsteAfspraak.getStatus();
			return AfspraakStatus.GEPLAND.equals(status) || AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType);
		}
		else
		{
			return false;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateContact(ClientContact contact, InstellingGebruiker loggedInInstellingGebruiker)
	{
		hibernateService.saveOrUpdate(contact);
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		logService.logGebeurtenis(LogGebeurtenis.CLIENTCONTACT_REGISTREREN, loggedInInstellingGebruiker, contact.getClient(),
			"van " + format.format(contact.getDatum()) + " gewijzigd");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderContact(ClientContact contact, InstellingGebruiker loggedInInstellingGebruiker)
	{
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		String melding = "van " + format.format(contact.getDatum()) + " verwijderd";
		Client client = contact.getClient();
		client.getContacten().remove(contact);
		hibernateService.delete(contact);
		hibernateService.saveOrUpdate(client);
		logService.logGebeurtenis(LogGebeurtenis.CLIENTCONTACT_REGISTREREN, loggedInInstellingGebruiker, client, melding);
	}

	@Override
	public List<AfmeldingType> getAvailableAfmeldoptiesMamma(Client client, boolean viaPortaalGevraagd)
	{
		return mammaAfmeldService.getBeschikbareAfmeldopties(client, viaPortaalGevraagd);
	}

	@Override
	public boolean heeftOpenMammaAfspraak(Client client)
	{
		MammaAfspraak laatsteAfspraak = null;
		if (client != null && client.getMammaDossier().getLaatsteScreeningRonde() != null
			&& client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			&& client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak() != null)
		{
			laatsteAfspraak = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();

			return laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND && laatsteAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) >= 0;
		}
		else
		{
			return false;
		}
	}

	@Override
	public boolean magNieuweIntakeAfspraakMakenNaHeraanmelding(ColonDossier colonDossier)
	{
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde == null)
		{
			return false;
		}

		IFOBTTest eersteOngunstigUitslag = ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteScreeningRonde);
		Integer uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();

		return laatsteAfspraak != null && AfspraakStatus.GEANNULEERD_AFMELDEN.equals(laatsteAfspraak.getStatus())
			&& eersteOngunstigUitslag != null && eersteOngunstigUitslag.getStatusDatum() != null
			&& !eersteOngunstigUitslag.getStatusDatum().before(DateUtil.toUtilDate(vandaag.minusDays(uitnodigingsinterval)));
	}

}
