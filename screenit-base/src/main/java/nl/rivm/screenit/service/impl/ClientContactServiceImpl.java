package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.exceptions.MammaStandplaatsVanPostcodeOnbekendException;
import nl.rivm.screenit.exceptions.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContact_;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixRedenUitnodiging;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BevestigingsType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.IntervalEenheidAanduiding;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.NieuweIfobtResultaat;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.logging.NieuweIFobtAanvraagLogEvent;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraakReservering;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.repository.algemeen.ClientContactRepository;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.DeelnamemodusDossierService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.service.colon.ColonHuisartsService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonTijdelijkAfmeldenJaartallenService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.service.mamma.MammaAfspraakReserveringService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Hibernate;
import org.hibernate.exception.GenericJDBCException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.orm.hibernate5.HibernateJdbcException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.model.ClientContactManier.AANVRAGEN_FORMULIEREN;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientContactSpecification.heeftClient;
import static nl.rivm.screenit.specification.algemeen.ClientContactSpecification.heeftClientId;
import static nl.rivm.screenit.specification.algemeen.ClientContactSpecification.heeftOpmerking;
import static nl.rivm.screenit.util.StringUtil.propertyChain;
import static org.springframework.data.domain.Sort.Direction.ASC;
import static org.springframework.data.domain.Sort.Direction.DESC;

@Component
@Transactional
public class ClientContactServiceImpl implements ClientContactService
{
	private static final Logger LOG = LoggerFactory.getLogger(ClientContactServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private ColonBaseAfspraakService afspraakService;

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
	private ColonBaseFITService fitService;

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

	@Autowired
	private DeelnamemodusDossierService deelnamemodusDossierService;

	@Autowired
	private ColonTijdelijkAfmeldenJaartallenService colonTijdelijkAfmeldenJaartallenService;

	@Autowired
	private MammaDigitaalContactService mammaDigitaalContactService;

	@Autowired
	private BaseGbaVraagService baseGbaVraagService;

	@Autowired
	private MammaAfspraakReserveringService afspraakReserveringService;

	@Autowired
	private ClientContactRepository clientContactRepository;

	@Override
	@Transactional(
		propagation = Propagation.REQUIRED,
		rollbackFor = { HibernateJdbcException.class, MammaTijdNietBeschikbaarException.class, GenericJDBCException.class,
			MammaStandplaatsVanPostcodeOnbekendException.class })
	public void saveClientContact(ClientContact contact, Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, Account account)
	{
		@SuppressWarnings("unchecked")
		var isInstellingGebruiker = Hibernate.getClass(account).isAssignableFrom(InstellingGebruiker.class);
		if (isInstellingGebruiker)
		{
			contact.setInstellingGebruiker((InstellingGebruiker) account);
		}
		List<ClientContactActie> actiesToDelete = new ArrayList<>();
		var client = contact.getClient();
		mammaDoelgroepGewijzigd(contact, account);
		contact = verwerkEerstHuisartsWijzigenActie(contact, actiesToDelete, account, extraOpslaanObjecten, isInstellingGebruiker);
		for (var actie : contact.getActies())
		{
			var extraOpslaanParams = extraOpslaanObjecten.get(actie.getType());
			switch (actie.getType())
			{
			case COLON_AANVRAGEN_NIEUWE_IFOBT:
				vraagNieuweIfobtAan(client, account);
				break;
			case OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
				baseGbaVraagService.vraagGbaGegevensOpnieuwAan(client, account, actie.getOpnieuwAanvragenClientgegevensReden());
				break;
			case TIJDELIJK_ADRES:
				tijdelijkAdres(account, client, actie, extraOpslaanParams);
				break;
			case AANPASSEN_AANHEF:
				aanhefWijzigen(account, client, actie, extraOpslaanParams);
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
			case DEELNAMEWENSEN:
				actie = deelnamewensenRegistreren(actie, client, extraOpslaanParams, account);
				break;
			case COLON_AFMELDEN:
			case CERVIX_AFMELDEN:
			case MAMMA_AFMELDEN:
				actie = afmelden(actiesToDelete, actie, client, extraOpslaanParams, account);
				break;
			case COLON_HERAANMELDEN:
			case CERVIX_HERAANMELDEN:
			case MAMMA_HERAANMELDEN:
				actie = heraanmelden(actiesToDelete, actie, extraOpslaanParams, account);
				break;
			case CERVIX_UITSTEL:
				actie = cervixUitstel(actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_ZAS_AANVRAGEN:
				actie = zasAanvragen(actie, client, extraOpslaanParams, account);
				break;
			case CERVIX_HERDRUK:
				actie = cervixHerdruk(actie, extraOpslaanParams, account);
				break;
			case COLON_HUISARTS_WIJZIGEN:
			case MAMMA_HUISARTS_WIJZIGEN:
				actie = null;
				break;
			case MAMMA_RONDE_FORCEREN:
				mammaBaseDossierService.rondeForceren(client);
				break;
			case MAMMA_AFSPRAAK_MAKEN_FORCEREN:
			case MAMMA_AFSPRAAK_MAKEN:
			case MAMMA_AFSPRAAK_WIJZIGEN:
				actie = mammaAfspraakMakenWijzigen(actie, client, extraOpslaanParams, account, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN.equals(actie.getType()),
					ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN.equals(actie.getType()));
				break;
			case MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS:
				actie = mammaMinderValideOnderzoekZiekenhuis(actie, client);
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
				LOG.warn("Actie type niet verwerkt/opgeslagen. Type: {}", actie.getType());
				break;
			}
			if (actie != null && isInstellingGebruiker)
			{
				hibernateService.saveOrUpdate(actie);
			}
		}
		for (var actieToDelete : actiesToDelete)
		{
			contact.getActies().remove(actieToDelete);
		}
		if (isInstellingGebruiker)
		{
			var uitgevoerdeActies = contact.getActies().stream().map(a -> a.getType().name()).collect(Collectors.joining(", "));
			logService.logGebeurtenis(LogGebeurtenis.CLIENTCONTACT_REGISTREREN, account, client, "Aangemaakt met vervolgstap(pen): " + uitgevoerdeActies);
			hibernateService.saveOrUpdate(contact);
		}
	}

	private ClientContactActie deelnamewensenRegistreren(ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams, Account account)
	{
		var deelnamewensDto = (DeelnamewensDto) extraOpslaanParams.get(ExtraOpslaanKey.DEELNAMEWENSEN);
		if (deelnamewensDto != null)
		{
			deelnamemodusDossierService.pasDeelnamewensToe(client, deelnamewensDto, account);
		}
		else
		{
			actie = null;
		}
		return actie;
	}

	private void mammaAnnuleerBeoordeling(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		var laatsteBeoordeling = MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(client);
		beoordelingService.valideerEnHerbeoordeelBeoordeling(laatsteBeoordeling, ingelogdeGebruiker);
	}

	private void mammaStuurVerzoekOmContact(Client client)
	{
		briefService.maakBvoBrief(client.getMammaDossier().getLaatsteScreeningRonde(), BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT);
	}

	private void mammaClientWilGeenVervolgOnderzoek(Client client)
	{
		var onderzoek = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteOnderzoek();
		mammaBaseOnderzoekService.vervolgOnderbrokenOnderzoeken(onderzoek);
	}

	private ClientContactActie cervixAanvraagVerwijderenUitslagBrief(ClientContactActie actie, Client client)
	{
		baseBriefService.maakBvoBrief(client.getCervixDossier().getLaatsteScreeningRonde(), BriefType.CERVIX_VERWIJDER_UITSLAG);
		return actie;
	}

	private ClientContactActie colonAanvraagVerwijderenUitslagBrief(ClientContactActie actie, Client client)
	{
		baseBriefService.maakBvoBrief(client.getColonDossier().getLaatsteScreeningRonde(), BriefType.COLON_VERWIJDER_UITSLAG);
		return actie;
	}

	private void mammaDoelgroepGewijzigd(ClientContact contact, Account account)
	{
		var client = contact.getClient();
		var mammaDoelgroepVastleggenActie = contact.getActies().stream().filter(c -> ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (mammaDoelgroepVastleggenActie.isPresent())
		{
			var mammaDossier = client.getMammaDossier();
			var doelgroep = mammaDossier.getDoelgroep();
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
		var afspraakBevestigingsType = (BevestigingsType) extraOpslaanParams.getOrDefault(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.GEEN);
		var smsStatus = (SmsStatus) extraOpslaanParams.getOrDefault(ExtraOpslaanKey.SMS_STATUS, SmsStatus.GEEN);

		var uitstel = (MammaUitstel) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_UITSTEL);
		baseKansberekeningService.resetPreferences();
		if (uitstel != null)
		{
			var briefAanmaken = BevestigingsType.BRIEF == afspraakBevestigingsType;
			mammaBaseUitstelService.saveUitstel(uitstel, briefAanmaken, account);
		}
		var afspraak = (MammaAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
		if (afspraak != null)
		{
			if (isGeforceerdeAfspraak)
			{
				var laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
				heropenRonde(laatsteScreeningRonde);
				var laatsteBeoordeling = laatsteScreeningRonde.getLaatsteOnderzoek().getLaatsteBeoordeling();
				if (laatsteBeoordeling != null)
				{
					beoordelingService.annuleerBeoordeling(laatsteBeoordeling);
				}
			}

			if (isAfspraakTijdBezet(afspraak, (IMammaAfspraakWijzigenFilter) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_AFSPRAAK_FILTER)))
			{
				throw new MammaTijdNietBeschikbaarException();
			}

			beoordelingService
				.zoekOpgeschorteBeoordelingInRonde(client.getMammaDossier().getLaatsteScreeningRonde(),
					MammaBeoordelingOpschortenReden.AANVULLENDE_BEELDEN_NODIG_SE, MammaBeoordelingOpschortenReden.PRIORS_VAN_BUITEN_BVO)
				.ifPresent(beoordeling ->
				{
					beoordelingService.setStatus(beoordeling, MammaBeoordelingStatus.OPGESCHORT_MET_AFSPRAAK);
					hibernateService.saveOrUpdate(beoordeling);
				});

			var vorigeAfspraakVerzetten = actie.getType().equals(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN);

			var nieuweAfspraak = mammaBaseAfspraakService.maakAfspraak(afspraak.getUitnodiging().getScreeningRonde(), afspraak.getCapaciteitBlok(), afspraak.getVanaf(),
				afspraak.getStandplaatsPeriode(), afspraak.getVerzettenReden(), vorigeAfspraakVerzetten, true, false, true, true, account,
				isGeforceerdeAfspraak, smsStatus);

			extraOpslaanParams.put(ExtraOpslaanKey.MAMMA_NIEUWE_AFSPRAAK_ID, nieuweAfspraak.getId());

			valideerToekomstigeAfspraken(afspraak);
			bepaalAfspraakBevestigingsActie(nieuweAfspraak, afspraakBevestigingsType, account, extraOpslaanParams);
			updateMobielNummer(client, account, extraOpslaanParams);

			var verzetClientenDto = new PlanningVerzetClientenDto();
			verzetClientenDto.clientIdSet.add(client.getId());
			verzetClientenDto.verzetStandplaatsPeriodeId = afspraak.getStandplaatsPeriode().getId();
			baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);

			afspraakReserveringService.verwijderReserveringenVoorClient(client);
		}

		return actie;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void mammaAfspraakBevestigingMakenVanuitClientPortaal(Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		var afspraak = hibernateService.get(MammaAfspraak.class, (Long) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_NIEUWE_AFSPRAAK_ID));
		afspraak.setSmsStatus((SmsStatus) extraOpslaanParams.get(ExtraOpslaanKey.SMS_STATUS));

		bepaalAfspraakBevestigingsActieVanuitClientPortaal(afspraak, client, extraOpslaanParams);
		updateMobielNummer(client, client, extraOpslaanParams);

		hibernateService.saveOrUpdate(afspraak);
	}

	private void updateMobielNummer(Client client, Account account, Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		if (SmsStatus.TE_VERSTUREN == extraOpslaanParams.get(ExtraOpslaanKey.SMS_STATUS))
		{
			var opgegevenMobielNummer = (String) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_HERINNERING_TELEFOONNUMMER);
			client.getPersoon().setTelefoonnummer1(opgegevenMobielNummer);

			clientService.saveContactGegevens(client, account);
		}
	}

	private void bepaalAfspraakBevestigingsActieVanuitClientPortaal(MammaAfspraak afspraak, Client client, Map<ExtraOpslaanKey, Object> opslaanObjecten)
	{
		var bevestigingsType = (BevestigingsType) opslaanObjecten.get(ExtraOpslaanKey.BEVESTIGINGS_TYPE);
		if (BevestigingsType.MAIL == bevestigingsType)
		{
			bepaalAfspraakBevestigingsActie(afspraak, bevestigingsType, client, opslaanObjecten);
		}
		else if (BevestigingsType.GEEN == bevestigingsType)
		{
			baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(afspraak.getUitnodiging().getScreeningRonde(),
				Collections.singletonList(BriefType.MAMMA_AFSPRAAK_VERZET));
		}
	}

	private void bepaalAfspraakBevestigingsActie(MammaAfspraak afspraak, BevestigingsType bevestigingsType, Account account, Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		if (BevestigingsType.BRIEF == bevestigingsType || MammaVerzettenReden.briefVerplicht(afspraak.getVerzettenReden()))
		{
			briefService.maakBvoBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET);
		}
		else if (BevestigingsType.MAIL == bevestigingsType)
		{
			var clientUitAfspraak = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
			var bevestigingsMailAdres = (String) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BEVESTIGING_MAIL_ADRES);
			clientUitAfspraak.getPersoon().setEmailadres(bevestigingsMailAdres);

			clientService.saveContactGegevens(clientUitAfspraak, account);
			mammaDigitaalContactService.sendBevestigAfspraakMail(afspraak);
		}
		else
		{
			baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(afspraak.getUitnodiging().getScreeningRonde(),
				Collections.singletonList(BriefType.MAMMA_AFSPRAAK_VERZET));
		}
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
		var heeftGeplandeAfspraak = new AtomicBoolean(false);
		afspraak.getUitnodiging().getScreeningRonde().getUitnodigingen().forEach(uitnodiging -> uitnodiging.getAfspraken().forEach(af ->
		{
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

	@Override
	public boolean isAfspraakTijdBezet(MammaAfspraak nieuweAfspraak, IMammaAfspraakWijzigenFilter afspraakWijzigenFilter)
	{
		var dossier = nieuweAfspraak.getUitnodiging().getScreeningRonde().getDossier();
		var capaciteitBlok = nieuweAfspraak.getCapaciteitBlok();
		var nieuweVanaf = nieuweAfspraak.getVanaf();
		if (clientOfAfspraakVanafMagNietInCapaciteitBlok(dossier, capaciteitBlok, nieuweVanaf))
		{
			LOG.trace("isAfspraakTijdBezet: Valt buiten blok/type");
			return true;
		}
		LOG.trace("isAfspraakTijdBezet: nieuweVanaf {} {}", nieuweVanaf, capaciteitBlok.getId());
		var client = dossier.getClient();
		var reserveringenVoorBlok = afspraakReserveringService.getActieveReserveringenVoorCapaciteitBlok(capaciteitBlok);

		if (afspraakHeeftBestaandeReservering(reserveringenVoorBlok, client, nieuweAfspraak))
		{
			return false;
		}

		var reserveringAanwezigOpMoment = isReserveringAanwezigOpMoment(reserveringenVoorBlok, nieuweVanaf);
		var isAfspraakInZelfdeCapaciteitBlokMetZelfdeVanafAanwezig = isAfspraakInZelfdeCapaciteitBlokMetZelfdeVanafAanwezig(capaciteitBlok, nieuweVanaf);

		if (isAfspraakInZelfdeCapaciteitBlokMetZelfdeVanafAanwezig || reserveringAanwezigOpMoment)
		{
			return isAfspraakTijdBeschikbaarVanuitKandidaatAfspraakOpties(nieuweAfspraak, afspraakWijzigenFilter, nieuweVanaf, capaciteitBlok);
		}
		else if (LOG.isTraceEnabled())
		{
			var afspraken = new StringBuilder();
			capaciteitBlok.getAfspraken().forEach(a -> afspraken.append(a.getVanaf() + ", "));
			LOG.info("isAfspraakTijdBezet: afspraken {}", afspraken);
		}
		return false;
	}

	private boolean isAfspraakInZelfdeCapaciteitBlokMetZelfdeVanafAanwezig(MammaCapaciteitBlok capaciteitBlok, Date nieuweVanaf)
	{
		return capaciteitBlok.getAfspraken().stream()
			.anyMatch(afspraak -> DateUtil.compareEquals(nieuweVanaf, afspraak.getVanaf()));
	}

	private boolean isReserveringAanwezigOpMoment(List<MammaAfspraakReservering> reserveringenVoorBlok, Date nieuweVanaf)
	{
		return reserveringenVoorBlok.stream()
			.anyMatch(reservering -> reservering.getVanaf().equals(DateUtil.toLocalDateTime(nieuweVanaf)));
	}

	private boolean clientOfAfspraakVanafMagNietInCapaciteitBlok(MammaDossier dossier, MammaCapaciteitBlok capaciteitBlok, Date nieuweVanaf)
	{
		return dossier.getTehuis() != null && !capaciteitBlok.getBlokType().equals(MammaCapaciteitBlokType.TEHUIS)
			|| dossier.getTehuis() == null && !capaciteitBlok.getBlokType().equals(MammaCapaciteitBlokType.REGULIER)
			|| DateUtil.compareBefore(nieuweVanaf, capaciteitBlok.getVanaf())
			|| !DateUtil.compareBefore(nieuweVanaf, capaciteitBlok.getTot());
	}

	private boolean isAfspraakTijdBeschikbaarVanuitKandidaatAfspraakOpties(MammaAfspraak nieuweAfspraak, IMammaAfspraakWijzigenFilter afspraakWijzigenFilter, Date nieuweVanaf,
		MammaCapaciteitBlok capaciteitBlok)
	{
		afspraakWijzigenFilter.setBuitenRegio(true);
		afspraakWijzigenFilter.setExtraOpties(true);
		var kandidaatAfspraken = mammaBaseAfspraakService
			.getKandidaatAfspraken(nieuweAfspraak.getUitnodiging().getScreeningRonde().getDossier().getClient(), afspraakWijzigenFilter);

		if (LOG.isTraceEnabled())
		{
			var afspraken = new StringBuilder();
			kandidaatAfspraken.forEach(a -> afspraken.append(a.getDatum() + " " + a.getTijd() + " " + a.getCapaciteitBlokId() + ", "));
			LOG.trace("isAfspraakTijdBezet: Kandaten {}", afspraken);
		}
		var tijd = DateUtil.toLocalTime(nieuweVanaf);
		var datum = DateUtil.toLocalDate(nieuweVanaf);

		return kandidaatAfspraken.stream().noneMatch(kandidaatAfspraakDto -> tijd.equals(kandidaatAfspraakDto.getTijd()) && datum.equals(kandidaatAfspraakDto.getDatum())
			&& capaciteitBlok.getId().equals(kandidaatAfspraakDto.getCapaciteitBlokId()));
	}

	private boolean afspraakHeeftBestaandeReservering(List<MammaAfspraakReservering> reserveringenVoorBlok, Client client, MammaAfspraak afspraak)
	{
		return reserveringenVoorBlok.stream()
			.anyMatch(reservering -> reservering.getClient().equals(client) && reservering.getVanaf().equals(DateUtil.toLocalDateTime(afspraak.getVanaf())));
	}

	private ClientContactActie mammaMinderValideNietMeerOnderzoekZiekenhuis(ClientContactActie actie, Client client, Account account)
	{
		var laatsteRonde = client.getMammaDossier().getLaatsteScreeningRonde();
		var laatsteBrief = laatsteRonde.getLaatsteBrief();
		laatsteRonde.setMinderValideOnderzoekZiekenhuis(false);
		if (laatsteBrief != null && BriefType.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS.equals(laatsteBrief.getBriefType()) && !BriefUtil.isGegenereerd(laatsteBrief))
		{
			baseBriefService.briefTegenhouden(laatsteBrief, account);
		}
		hibernateService.saveOrUpdateAll(laatsteBrief, laatsteRonde);
		return actie;
	}

	private ClientContactActie mammaMinderValideOnderzoekZiekenhuis(ClientContactActie actie, Client client)
	{
		var laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();

		var laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
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

		baseBriefService.maakBvoBrief(laatsteScreeningRonde, BriefType.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS);
		hibernateService.saveOrUpdate(laatsteScreeningRonde);

		return actie;
	}

	private ClientContactActie mammaInfobriefProthesen(ClientContactActie actie, Client client, Account account)
	{
		baseBriefService.maakBvoBrief(client.getMammaDossier().getLaatsteScreeningRonde(), BriefType.MAMMA_INFOBRIEF_PROTHESEN);
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
		var huisartsWijzigenActie = contact.getActies().stream().filter(c -> ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (huisartsWijzigenActie.isPresent())
		{
			var actie = huisartsWijzigenActie.get();
			var extraOpslaanParams = extraOpslaanObjecten.get(ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN);
			actie = wijzigMammaHuisarts(account, actie, actiesToDelete, extraOpslaanParams);

			if (actie != null && isInstellingGebruiker)
			{
				hibernateService.saveOrUpdate(actie);
			}
		}
	}

	private void verwerkEerstColonHuisartsWijzigenActie(ClientContact contact, List<ClientContactActie> actiesToDelete, Account account,
		Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, boolean isInstellingGebruiker)
	{
		var huisartsWijzigenActie = contact.getActies().stream().filter(c -> ClientContactActieType.COLON_HUISARTS_WIJZIGEN.equals(c.getType()))
			.findFirst();
		if (huisartsWijzigenActie.isPresent())
		{
			var actie = huisartsWijzigenActie.get();
			var extraOpslaanParams = extraOpslaanObjecten.get(ClientContactActieType.COLON_HUISARTS_WIJZIGEN);
			var isAfspraakActieAanwezig = contact.getActies().stream().anyMatch(
				c -> (ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN.equals(c.getType()) || ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN.equals(c.getType())));
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
		var ronde = (ColonScreeningRonde) extraOpslaanParams.get(ExtraOpslaanKey.COLON_HUISARTS);
		var huisartsBerichtenVerzenden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.COLON_HUISARTSBERICHTEN_VERZENDEN);

		var isGewijzigd = colonHuisartsService.koppelHuisarts(ronde.getColonHuisarts(), ronde, account);

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

	private ClientContactActie wijzigMammaHuisarts(Account account, ClientContactActie actie, List<ClientContactActie> actiesToDelete,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		var ronde = (MammaScreeningRonde) extraOpslaanParams.get(ExtraOpslaanKey.MAMMA_HUISARTS);

		var isGewijzigd = mammaHuisartsService.koppelHuisarts(ronde.getHuisarts(), ronde, account);

		if (!isGewijzigd)
		{
			actiesToDelete.add(actie);
			actie = null;
		}
		return actie;
	}

	private ClientContactActie cervixHerdruk(ClientContactActie actie, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		var cervixBrief = (CervixBrief) extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_HERDRUK_BRIEF);
		cervixBrief.setAangevraagdeHerdruk(true);
		briefHerdrukkenService.opnieuwAanmaken(cervixBrief, account);
		return actie;
	}

	private ClientContactActie zasAanvragen(ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		var zasAangevraagdDoorClient = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.ZAS_DOOR_CLIENT_AANGEVRAAGD));
		var zasUitstelPeriodeNemen = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_UITSTEL));
		var redenVoorAanvraagZas = (CervixRedenUitnodiging) extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_ZAS_AANVRAAG_REDEN);
		var uitnodiging = factory.maakZasUitnodiging(client, account, zasUitstelPeriodeNemen, zasAangevraagdDoorClient);
		uitnodiging.setRedenUitnodiging(redenVoorAanvraagZas);
		return actie;
	}

	private ClientContactActie cervixUitstel(ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		var uitstel = (CervixUitstel) extraOpslaanParams.get(ExtraOpslaanKey.CERVIX_UITSTEL);
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
		var tijdelijkAdres = (TijdelijkAdres) extraOpslaanParams.get(ExtraOpslaanKey.TIJDELIJK_ADRES);
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
		tijdelijkAdres.setPostcodeCoordinaten(coordinatenService.getCoordinaten(tijdelijkAdres));
		hibernateService.saveOrUpdateAll(tijdelijkAdres, client);
		logService.logGebeurtenis(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES, account, client);
	}

	private ClientContactActie aanhefWijzigen(Account account, Client client, ClientContactActie actie, Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		var aanhef = (Aanhef) extraOpslaanParams.get(ExtraOpslaanKey.AANHEF);
		if (Aanhef.aanhefVormenClienten().contains(aanhef))
		{
			saveAanhef(account, client, aanhef);
		}
		else
		{
			actie = null;
		}
		return actie;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveAanhef(Account account, Client client, Aanhef aanhef)
	{
		var persoon = client.getPersoon();
		persoon.setAanhef(aanhef);
		hibernateService.saveOrUpdate(persoon);
		logService.logGebeurtenis(LogGebeurtenis.WIJZIG_AANHEF, account, client, String.format("Aanhef gewijzigd naar: %s", aanhef.getNaam()));
	}

	private ClientContactActie nieuweAfspraakMaken(Account account, Client client, List<ClientContactActie> actiesToDelete, ClientContactActie actie,
		Map<ExtraOpslaanKey, Object> extraOpslaanParams)
	{
		var kanNieuweAfspraakAanmaken = false;
		if (extraOpslaanParams != null && extraOpslaanParams.size() >= 1)
		{
			var nieuweAfspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
			var briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
			var briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
			var afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
			afspraakService.maakNieuweAfspraak(client, nieuweAfspraak, !Boolean.FALSE.equals(briefTegenhouden), Boolean.TRUE.equals(afspraakUitRooster), briefType, account);

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

	private ClientContactActie heraanmelden(List<ClientContactActie> actiesToDelete, ClientContactActie actie, Map<ExtraOpslaanKey, Object> extraOpslaanParams,
		Account account)
	{
		var herAanTeMeldenAfmelding = (Afmelding) extraOpslaanParams.get(ExtraOpslaanKey.HERAANMELDING);
		if (herAanTeMeldenAfmelding == null)
		{
			actiesToDelete.add(actie);
			return null;
		}
		else
		{
			if (herAanTeMeldenAfmelding.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON && extraOpslaanParams.size() >= 1)
			{
				var afspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
				var afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
				var briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
				var briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
				var clientWilNieuweUitnodiging = Boolean.TRUE.equals(extraOpslaanParams.get(ExtraOpslaanKey.NIEUWE_UITNODIGING));
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
		if (extraOpslaanParams != null && !extraOpslaanParams.isEmpty() && extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR) != null)
		{
			var toegevoegdeBezwaar = (BezwaarMoment) extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR);
			var vragenOmHandtekening = (boolean) extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR_VRAGEN_OM_HANDTEKENING);
			if (AANVRAGEN_FORMULIEREN.contains(toegevoegdeBezwaar.getManier()))
			{
				bezwaarService.maakBezwaarAanvraag(client, vragenOmHandtekening, toegevoegdeBezwaar.getManier().getBriefType());
			}
			else
			{

				hibernateService.saveOrUpdate(toegevoegdeBezwaar);
				@SuppressWarnings("unchecked")
				var groupWrappers = (List<BezwaarGroupViewWrapper>) extraOpslaanParams.get(ExtraOpslaanKey.BEZWAAR_WRAPPERS);
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
		var afmelding = (Afmelding) extraOpslaanParams.get(ExtraOpslaanKey.AFMELDING);
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
			var afspraak = (ColonIntakeAfspraak) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK);
			var nieuweAfspraakStatus = (ColonAfspraakStatus) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_STATUS);
			var briefType = (BriefType) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF);
			var briefTegenhouden = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN);
			var afspraakUitRooster = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.AFSPRAAK_UIT_ROOSTER);
			var doorverwezenMedischeRedenen = (Boolean) extraOpslaanParams.get(ExtraOpslaanKey.COLON_VERWIJZING_MEDISCHE_REDENEN_INFOLIJN);
			if (ColonAfspraakStatus.VERPLAATST.equals(nieuweAfspraakStatus))
			{
				afspraakService.verplaatsAfspraak(afspraak, account, briefType, !Boolean.FALSE.equals(briefTegenhouden), Boolean.TRUE.equals(afspraakUitRooster),
					Boolean.TRUE.equals(doorverwezenMedischeRedenen));
			}
			else if (ColonAfspraakStatus.GEANNULEERD_VIA_INFOLIJN.equals(nieuweAfspraakStatus))
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
		var clientOverleden = clientService.isClientOverleden(client);
		var clientInBuitenland = clientService.clientInBuitenland(client);

		if (clientOverleden)
		{
			return availableActies;
		}

		var toonVervangendeTekstMammaOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false);
		var toonVervangendeTekstCervixOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(),
			false);
		var toonVervangendeTekstColonOpClientPortaal = viaClientportaal
			&& preferenceService.getBoolean(PreferenceKey.COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), false);

		var behoortTotDoelgroepColon = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.COLON);
		var behoortTotDoelgroepCervix = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.CERVIX);
		var behoortTotDoelgroepMamma = doelgroepService.behoortTotDoelgroep(client, Bevolkingsonderzoek.MAMMA);
		List<ClientContactActieType> clientContactActieTypes = new ArrayList<>();

		if (clientInBuitenland)
		{
			clientContactActieTypes.add(ClientContactActieType.GEEN);
			clientContactActieTypes.add(ClientContactActieType.INZAGE_PERSOONSGEGEVENS);
			clientContactActieTypes.add(ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN);
		}
		else
		{
			clientContactActieTypes.addAll(Arrays.asList(ClientContactActieType.values()));
		}

		for (var actieType : clientContactActieTypes)
		{

			if (actieType.equals(ClientContactActieType.GEEN) && viaClientportaal)
			{
				continue;
			}
			if (Bevolkingsonderzoek.heeftAlleBevolkingsonderzoeken(actieType.getBevolkingsonderzoeken())
				&& availableGen(client, actieType))
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepColon
				&& Bevolkingsonderzoek.alleenDarmkanker(actieType.getBevolkingsonderzoeken())
				&& availableColon(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstColonOpClientPortaal)
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepCervix
				&& Bevolkingsonderzoek.alleenBaarmoederhalskanker(actieType.getBevolkingsonderzoeken())
				&& availableCervix(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstCervixOpClientPortaal
				&& client.getCervixDossier().getDeelnamemodus() != Deelnamemodus.SELECTIEBLOKKADE)
			{
				availableActies.add(actieType);
			}
			else if (behoortTotDoelgroepMamma
				&& Bevolkingsonderzoek.alleenBorstkanker(actieType.getBevolkingsonderzoeken())
				&& availableMamma(client, actieType, viaClientportaal)
				&& !toonVervangendeTekstMammaOpClientPortaal
				&& client.getMammaDossier().getDeelnamemodus() != Deelnamemodus.SELECTIEBLOKKADE)
			{
				availableActies.add(actieType);
			}
		}
		return availableActies;
	}

	@Override
	public boolean availableActiesBevatBenodigdeActie(Client client, ClientContactActieType benodigdeActie)
	{
		var beschikbareActies = getAvailableActies(client, true);
		return beschikbareActies.contains(benodigdeActie);
	}

	@Override
	public Long countClientContactenMetOpmerking(Long clientId)
	{
		return clientContactRepository.count(heeftClientId(clientId).and(heeftOpmerking()));
	}

	private boolean availableGen(Client client, ClientContactActieType actieType)
	{
		switch (actieType)
		{
		case GEEN:
		case AANPASSEN_AANHEF:
		case TIJDELIJK_ADRES:
		case DEELNAMEWENSEN:
		case INZAGE_PERSOONSGEGEVENS:
		case CERVIX_DEELNAME_BUITEN_BVO_BMHK:
		case BEZWAAR:
			return true;
		case OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
			return GbaStatus.INDICATIE_AANWEZIG.equals(client.getGbaStatus());

		default:
			return false;
		}
	}

	private boolean availableColon(Client client, ClientContactActieType actieType, boolean viaClientportaal)
	{
		var colonDossier = client.getColonDossier();

		boolean isDossierAangemeld = colonDossier.getAangemeld();
		var isDossierTijdelijkUitgesteld = colonDossier.getVolgendeUitnodiging() != null && colonDossier.getVolgendeUitnodiging().getDatumVolgendeRonde() != null;

		ColonAfmelding laatsteDefinitieveAfmelding;
		laatsteDefinitieveAfmelding = colonDossier.getLaatsteAfmelding();
		var isLaatsteDefinitieveAfmelding = laatsteDefinitieveAfmelding != null;

		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		var heeftNietVerlopenLaatsteScreeningRonde = ColonScreeningRondeUtil.isLaatsteScreeningRondeNietVerlopen(laatsteScreeningRonde);
		var isLaatsteRondeGeldigEnAangemeld = ColonScreeningRondeUtil.isLaatsteScreeningRondGeldigEnAangemeld(laatsteScreeningRonde);

		var isLaatsteDefinitieveAfmeldingVerwerkt = isLaatsteDefinitieveAfmelding && AanvraagBriefStatus.VERWERKT.equals(laatsteDefinitieveAfmelding.getAfmeldingStatus());
		var isLaatsteDefinitieveAfmeldingHeraangemeld = isLaatsteDefinitieveAfmeldingVerwerkt
			&& AanvraagBriefStatus.VERWERKT.equals(laatsteDefinitieveAfmelding.getHeraanmeldStatus());

		var isLaatsteAfmeldingProefBevolking = isLaatsteDefinitieveAfmeldingVerwerkt
			&& ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(laatsteDefinitieveAfmelding.getReden());

		var magAfspaakWijzigenAfzeggen = client.getAfspraken().stream().anyMatch(afspraak -> afspraakService.magWijzigenAfzeggen(afspraak));

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
				|| isDossierAangemeld && heeftNietVerlopenLaatsteScreeningRonde && !isLaatsteRondeGeldigEnAangemeld
				|| isDossierTijdelijkUitgesteld;
		case COLON_OPEN_UITNODIGING:
			if (heeftNietVerlopenLaatsteScreeningRonde)
			{
				var isErEenOpenUitnodiging = laatsteScreeningRonde.getOpenUitnodiging() != null;
				var isDeOpenUitnodigingGemerged = isErEenOpenUitnodiging && laatsteScreeningRonde.getOpenUitnodiging().getUitnodigingsBrief() != null
					&& laatsteScreeningRonde.getOpenUitnodiging().getUitnodigingsBrief().getMergedBrieven() != null;
				var isErEenLaatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak() != null;
				var isDeLaatsteAfspraakGeannuleerd = isErEenLaatsteAfspraak && ColonAfspraakStatus.isGeannuleerd(laatsteScreeningRonde.getLaatsteAfspraak().getStatus());
				var isColonDossierIsAfgemeldViaAfmelding = !colonDossier.getAangemeld();
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
		var dossier = client.getCervixDossier();

		if (dossier != null)
		{
			var definitiefAfmelden = true;
			var definitiefHeraanmelden = false;
			var eenmaligAfmelden = false;
			var eenmaligHeraanmelden = false;
			var herdruk = false;
			var uitstel = false;
			var zasAanvragen = false;

			var laatsteDefinitieveAfmelding = dossier.getLaatsteAfmelding();
			definitiefAfmelden = dossier.getStatus() == DossierStatus.ACTIEF
				&& (laatsteDefinitieveAfmelding == null || laatsteDefinitieveAfmelding.getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT || viaClientportaal);
			definitiefHeraanmelden = laatsteDefinitieveAfmelding != null
				&& laatsteDefinitieveAfmelding.getAfmeldingStatus() == AanvraagBriefStatus.VERWERKT
				&& laatsteDefinitieveAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT;

			var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
			var leeftijd = DateUtil.getLeeftijd(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()), currentDateSupplier.getLocalDate());
			var startVolgendeRondeNogNietVerstreken = dossier.getVolgendeRondeVanaf() == null ||
				dossier.getVolgendeRondeVanaf() != null && dossier.getVolgendeRondeVanaf().after(currentDateSupplier.getDate());
			if (laatsteScreeningRonde != null && (leeftijd < CervixLeeftijdcategorie.minimumLeeftijd() || startVolgendeRondeNogNietVerstreken))
			{
				eenmaligHeraanmelden = !laatsteScreeningRonde.getAangemeld() && dossier.getAangemeld();

				if (laatsteScreeningRonde.getStatus() == ScreeningRondeStatus.LOPEND)
				{
					eenmaligAfmelden = laatsteScreeningRonde.getStatus() == ScreeningRondeStatus.LOPEND;
					uitstel = true;

					var laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(laatsteScreeningRonde, false);
					if (laatsteUitnodiging != null)
					{
						herdruk = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(laatsteUitnodiging.getBrief())
							&& cervixMagNieuweUitnodigingAanvragen(dossier);
					}
					if (laatsteScreeningRonde.getMonsterHpvUitslag() == null && !laatsteScreeningRonde.getUitnodigingen().isEmpty())
					{
						var laatsteZasUitnodiging = laatsteScreeningRonde.getLaatsteZasUitnodiging();
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
			case CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
				if (laatsteScreeningRonde != null)
				{
					var laatsteMonsterKanUitslagVerwijderdWorden = laatsteScreeningRonde.getLaatsteUitnodiging() != null
						&& laatsteScreeningRonde.getLaatsteUitnodiging().getMonster() != null
						&& laatsteScreeningRonde.getLaatsteUitnodiging().getMonster().equals(cervixUitnodigingService.getUitnodigingMagVerwijderdWorden(laatsteScreeningRonde));
					var laatsteZasKanVerwijderdWorden = laatsteScreeningRonde.getLaatsteZasUitnodiging() != null
						&& laatsteScreeningRonde.getLaatsteZasUitnodiging().getMonster() != null
						&& laatsteScreeningRonde.getLaatsteZasUitnodiging().getMonster().equals(cervixUitnodigingService.getUitnodigingMagVerwijderdWorden(laatsteScreeningRonde));
					return laatsteMonsterKanUitslagVerwijderdWorden || laatsteZasKanVerwijderdWorden;
				}
				return false;
			default:
				return false;
			}
		}
		return false;
	}

	private boolean availableMamma(Client client, ClientContactActieType actieType, boolean viaClientportaal)
	{
		boolean afspraakMaken;
		var verzetten = false;
		var minderValideOnderzoekZiekenhuis = false;
		var minderValideNietMeerOnderzoekZiekenhuis = false;
		var magClientVervolgOnderzoekAfwijzen = false;
		var magClientVerzoekVoorContactOpsturen = false;
		var infoBriefProthesenKlaarzetten = false;
		var magDoelgroepWijzigen = false;
		var magBeoordelingAnnuleren = false;

		var dossier = client.getMammaDossier();
		var laatsteRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteRonde != null)
		{
			var laatsteUitnodiging = laatsteRonde.getLaatsteUitnodiging();
			var laatsteAfspraak = laatsteUitnodiging != null ? laatsteUitnodiging.getLaatsteAfspraak() : null;

			if (laatsteRonde.getStatus() == ScreeningRondeStatus.LOPEND)
			{
				minderValideOnderzoekZiekenhuis = dossier.getDoelgroep() == MammaDoelgroep.MINDER_VALIDE && laatsteUitnodiging != null
					&& !laatsteRonde.getMinderValideOnderzoekZiekenhuis() && (laatsteAfspraak == null || laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND
					|| MammaAfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus()));
				minderValideNietMeerOnderzoekZiekenhuis = laatsteRonde.getMinderValideOnderzoekZiekenhuis();
				var vorigeInfobriefProthesen = laatsteRonde.getBrieven().stream().filter(brief -> BriefType.MAMMA_INFOBRIEF_PROTHESEN.equals(brief.getBriefType()))
					.max(Comparator.comparing(Brief::getCreatieDatum))
					.orElse(null);

				infoBriefProthesenKlaarzetten = vorigeInfobriefProthesen == null
					|| BriefUtil.isMergedBrievenGeprint(vorigeInfobriefProthesen);
			}
			magDoelgroepWijzigen = !minderValideNietMeerOnderzoekZiekenhuis;
			if (dossier.getStatus() != DossierStatus.INACTIEF)
			{
				var laatsteOnderzoek = laatsteRonde.getLaatsteOnderzoek();
				var laatsteBeoordeling = laatsteOnderzoek != null ? MammaScreeningRondeUtil.getLaatsteBeoordeling(laatsteOnderzoek) : null;
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
		case MAMMA_UITSTELLEN:
			return (afspraakMaken || verzetten) && mammaBaseAfspraakService.magUitstellen(dossier);
		default:
			return false;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public NieuweIfobtResultaat vraagNieuweIfobtAan(Client client, Account account)
	{
		var laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();

		var laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null)
		{
			var test = laatsteUitnodiging.getGekoppeldeTest();
			if (test != null && !IFOBTTestStatus.VERWIJDERD.equals(test.getStatus()) && !IFOBTTestStatus.VERLOREN.equals(test.getStatus()))
			{
				fitService.markeerBuisAlsVerloren(laatsteUitnodiging);
			}
			var nieuweUitnodiging = colonUitnodigingsService.cloneUitnodiging(laatsteUitnodiging, true);
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
		var resultaat = NieuweIfobtResultaat.AANGEVRAAGD;

		var logEvent = new NieuweIFobtAanvraagLogEvent();
		logEvent.setClient(client);
		logEvent.setResultaat(resultaat);
		logService.logGebeurtenis(LogGebeurtenis.NIEUWE_IFOBT_AANGEVRAAGD, logEvent, account, client, Bevolkingsonderzoek.COLON);

		return resultaat;
	}

	private boolean magNieuweIfobtAanvragen(Client client, boolean isHeraanmelding)
	{
		var resultaat = true;
		var colonDossier = client.getColonDossier();

		if (colonDossier.getLaatsteScreeningRonde() == null)
		{
			return false;
		}
		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		var eenmaligeOfTijdelijkeAfmelding = laatsteScreeningRonde.getLaatsteAfmelding();
		var definitiefAfmelding = colonDossier.getLaatsteAfmelding();

		var magNieuweUitnodigingAanvragen = eenmaligeOfTijdelijkeAfmelding != null && Boolean.TRUE.equals(eenmaligeOfTijdelijkeAfmelding.getRondeGesloten())
			&& Boolean.FALSE.equals(eenmaligeOfTijdelijkeAfmelding.getRondeHeropend())
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
	public List<AfmeldingType> getAvailableAfmeldoptiesColon(Client client, boolean viaClientenportaalGevraagd)
	{
		var afmeldTypes = new ArrayList<AfmeldingType>();
		var colonDossier = client.getColonDossier();
		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		var heeftNietVerlopenLaatsteScreeningRonde = laatsteScreeningRonde != null && !(ScreeningRondeStatus.AFGEROND.equals(laatsteScreeningRonde.getStatus())
			&& Constants.RONDE_AFROND_REDEN_BUITEN_DOELGROEP.equals(laatsteScreeningRonde.getAfgerondReden()));

		addEenmaligAfmeldenOptiesColon(afmeldTypes, laatsteScreeningRonde, heeftNietVerlopenLaatsteScreeningRonde);

		addTijdelijkeOfDefinitiefAfmeldenOptiesColon(afmeldTypes, client, viaClientenportaalGevraagd, heeftNietVerlopenLaatsteScreeningRonde);
		return afmeldTypes;
	}

	private void addEenmaligAfmeldenOptiesColon(List<AfmeldingType> afmeldTypes, ColonScreeningRonde laatsteScreeningRonde, boolean heeftNietVerlopenLaatsteScreeningRonde)
	{
		if (heeftNietVerlopenLaatsteScreeningRonde && Boolean.TRUE.equals(laatsteScreeningRonde.getAangemeld()))
		{
			if (laatsteScreeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND)
			{
				var ifobtTest = laatsteScreeningRonde.getLaatsteIFOBTTest();
				if (FITTestUtil.isGunstig(ifobtTest))
				{
					afmeldTypes.add(AfmeldingType.EENMALIG);
				}
			}
			else
			{
				var laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
				var geenColoscopie = true;
				if (laatsteAfspraak != null)
				{
					var conclusie = laatsteAfspraak.getConclusie();
					geenColoscopie = conclusie == null || conclusie.getType() != ColonConclusieType.COLOSCOPIE;
				}
				if (geenColoscopie)
				{
					afmeldTypes.add(AfmeldingType.EENMALIG);
				}
			}
		}
	}

	private void addTijdelijkeOfDefinitiefAfmeldenOptiesColon(List<AfmeldingType> afmeldTypes, Client client, boolean viaClientenportaalGevraagd,
		boolean heeftNietVerlopenLaatsteScreeningRonde)
	{
		var colonDossier = client.getColonDossier();
		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		var laatsteDefinitieveAfmelding = colonDossier.getLaatsteAfmelding();
		var heeftAangevraagdeDefinitieveAfmelding = AfmeldingUtil.isAangevraagdeDefinitieveAfmelding(laatsteDefinitieveAfmelding);
		var heeftAangevraagdeTijdelijkeAfmelding =
			heeftNietVerlopenLaatsteScreeningRonde && AfmeldingUtil.isAangevraagdeTijdelijkeAfmelding(laatsteScreeningRonde.getLaatsteAfmelding());
		var heeftAfgerondeDefinitieveAfmelding = AfmeldingUtil.isAfgerondeDefinitieveAfmelding(laatsteDefinitieveAfmelding);
		var heeftAfgerondeTijdelijkeAfmelding = heeftNietVerlopenLaatsteScreeningRonde && AfmeldingUtil.isAfgerondeTijdelijkeAfmelding(laatsteScreeningRonde.getLaatsteAfmelding());

		if (Boolean.TRUE.equals(colonDossier.getAangemeld()) && (laatsteScreeningRonde == null || heeftNietVerlopenLaatsteScreeningRonde)
			&& !heeftAfgerondeDefinitieveAfmelding && !heeftAfgerondeTijdelijkeAfmelding
			&& (viaClientenportaalGevraagd || !heeftAangevraagdeTijdelijkeAfmelding && !heeftAangevraagdeDefinitieveAfmelding))
		{
			var wordtUitgenodigdVolgendeRonde =
				colonDossier.getVolgendeUitnodiging() == null || colonDossier.getVolgendeUitnodiging().getInterval().getEenheid() != IntervalEenheidAanduiding.GEEN;
			var heeftBeschikbareTijdelijkAfmeldOpties = !colonTijdelijkAfmeldenJaartallenService.bepaalMogelijkeAfmeldJaren(client).isEmpty();
			if (colonDossier.getColonVooraankondiging() != null
				&& wordtUitgenodigdVolgendeRonde
				&& heeftBeschikbareTijdelijkAfmeldOpties
				&& heeftNietVerlopenLaatsteScreeningRonde)
			{
				afmeldTypes.add(AfmeldingType.TIJDELIJK);
			}
			afmeldTypes.add(AfmeldingType.DEFINITIEF);
		}
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
		var dossier = client.getCervixDossier();
		return dossier.getAangemeld()
			&& (dossier.getLaatsteAfmelding() == null || dossier.getLaatsteAfmelding().getAfmeldingStatus() != AanvraagBriefStatus.BRIEF || viaPortaalGevraagd);
	}

	private boolean cervixIsEenmaligAfmeldenMogelijk(Client client)
	{
		var dossier = client.getCervixDossier();
		var ronde = dossier.getLaatsteScreeningRonde();
		return ronde != null && ronde.getAangemeld() && ronde.getStatus() == ScreeningRondeStatus.LOPEND;
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
		var uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		var vandaag = currentDateSupplier.getLocalDate();
		return DateUtil.compareBefore(ronde.getCreatieDatum(), DateUtil.toUtilDate(vandaag.minusDays(uitnodigingsinterval)));
	}

	@Override
	public boolean defaultNieuweUitnodigingAanvragen(Dossier dossier)
	{
		var nieuweUitnodiging = false;
		var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		switch (dossier.getBevolkingsonderzoek())
		{
		case COLON:
			if (dossier.getScreeningRondes().size() == 1 && laatsteScreeningRonde != null)
			{
				var laatsteColonUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();
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
		var ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null && ronde.getUitstrijkjeVervolgonderzoekUitslag() == null)
		{
			if (ronde.getUitnodigingVervolgonderzoek() != null)
			{
				return true;
			}

			if (ronde.getUitstrijkjeCytologieUitslag() == null)
			{
				var monsterHpvUitslagHpvUitslag = ronde.getMonsterHpvUitslag();
				if (monsterHpvUitslagHpvUitslag != null)
				{
					return monsterHpvUitslagHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF;
				}
				else
				{
					return true;
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

			var conclusie = laatsteAfspraak.getConclusie();
			if (conclusie != null)
			{
				colonConclusieType = conclusie.getType();
			}

			var status = laatsteAfspraak.getStatus();
			return ColonAfspraakStatus.GEPLAND.equals(status) || ColonAfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType);
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
		var format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		logService.logGebeurtenis(LogGebeurtenis.CLIENTCONTACT_REGISTREREN, loggedInInstellingGebruiker, contact.getClient(),
			"van " + format.format(contact.getDatum()) + " gewijzigd");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderContact(ClientContact contact, InstellingGebruiker loggedInInstellingGebruiker)
	{
		var format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		var melding = "van " + format.format(contact.getDatum()) + " verwijderd";
		var client = contact.getClient();
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
		MammaAfspraak laatsteAfspraak;
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
		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde == null)
		{
			return false;
		}

		var eersteOngunstigUitslag = ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteScreeningRonde);
		var uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		var vandaag = currentDateSupplier.getLocalDate();
		var laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();

		return laatsteAfspraak != null && ColonAfspraakStatus.GEANNULEERD_AFMELDEN.equals(laatsteAfspraak.getStatus())
			&& eersteOngunstigUitslag != null && eersteOngunstigUitslag.getStatusDatum() != null
			&& !eersteOngunstigUitslag.getStatusDatum().before(DateUtil.toUtilDate(vandaag.minusDays(uitnodigingsinterval)));
	}

	@Override
	public List<ClientContact> getClientContacten(Client client, long first, long count, String sortProperty, boolean ascending)
	{
		var spec = heeftClient(client);

		if (sortProperty != null)
		{
			return clientContactRepository.findWith(spec, q ->
				q.sortBy(Sort.by(ascending ? ASC : DESC, sortProperty), ClientContactServiceImpl::sorteerClientContacten)).all(first, count);
		}
		return clientContactRepository.findAll(spec);
	}

	private static Order sorteerClientContacten(Sort.Order order, Root<ClientContact> r, CriteriaBuilder cb)
	{
		if (order.getProperty().startsWith(propertyChain(ClientContact_.INSTELLING_GEBRUIKER, InstellingGebruiker_.MEDEWERKER)))
		{
			var instellingGebruikerJoin = join(r, ClientContact_.instellingGebruiker);
			join(instellingGebruikerJoin, InstellingGebruiker_.medewerker);
		}
		return null;
	}

}
