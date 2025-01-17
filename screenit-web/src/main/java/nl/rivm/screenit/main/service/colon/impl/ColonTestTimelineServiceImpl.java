package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.RetourzendingService;
import nl.rivm.screenit.main.service.TestTimelineTimeService;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.service.colon.ColonVervolgonderzoekKeuzesDto;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class ColonTestTimelineServiceImpl implements ColonTestTimelineService
{

	public static final int DEFAULT_TIJD_TUSSEN_VERSTUURD_EN_UITSLAG_ONTVANGEN = 8;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private TestService testService;

	@Autowired
	private TestTimelineTimeService testTimelineTimeService;

	@Autowired
	private ColonBaseFITService fitService;

	@Autowired
	private ColonStudietestService studietestService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private DossierService dossierService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private RetourzendingService retourzendingService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private ColonVerwerkVerslagService verwerkVerslagService;

	@Autowired
	private BaseVerslagService verslagService;

	@Autowired
	private ColonDossierBaseService colonDossierBaseService;

	@Autowired
	private ColonDossierService colonDossierService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		var keuzes = new ArrayList<TestVervolgKeuzeOptie>();
		if (client.getColonDossier().getLaatsteScreeningRonde() != null
			&& client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{
			for (var ronde : client.getColonDossier().getScreeningRondes())
			{
				if (ronde.getLaatsteUitnodiging() != null)
				{
					keuzeAntwoordFormulierEnFit(keuzes, ronde);
					keuzeIntakeAfspraak(keuzes, ronde);
					keuzeIntakeAfspraakConclusie(keuzes, ronde);
					keuzeHerinneringFit(keuzes, ronde);
					keuzeRetourZending(keuzes, ronde);
				}
			}
			if (client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getGekoppeldeTest() == null)
			{
				keuzes.add(TestVervolgKeuzeOptie.UITNODIGING_POPUP);
			}
		}
		keuzes.add(TestVervolgKeuzeOptie.VERZET_TIJD);
		keuzes.add(TestVervolgKeuzeOptie.EINDE_SCREENINGSRONDE);
		return keuzes;
	}

	private void keuzeHerinneringFit(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.HERINNERING))
		{
			for (var test : ronde.getIfobtTesten())
			{
				if (!test.isHerinnering() && test.getStatus().equals(IFOBTTestStatus.ACTIEF))
				{
					keuzes.add(TestVervolgKeuzeOptie.HERINNERING);
					return;
				}
			}
		}
	}

	private void keuzeAntwoordFormulierEnFit(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.IFOBT))
		{
			for (var test : ronde.getIfobtTesten())
			{
				if (test.getUitslag() == null)
				{
					keuzes.add(TestVervolgKeuzeOptie.IFOBT);
					return;
				}
			}
		}
	}

	private void keuzeRetourZending(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.RETOURZENDING))
		{
			for (var test : ronde.getIfobtTesten())
			{
				if (test.getType().equals(IFOBTType.GOLD) && retourzendingService.isValideColonUitnodiging(test.getColonUitnodiging()) == null)
				{
					keuzes.add(TestVervolgKeuzeOptie.RETOURZENDING);
					return;
				}
			}
		}
	}

	private void keuzeIntakeAfspraak(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (ColonScreeningRondeUtil.zijnErOngunstigeIfobts(ronde))
		{
			if (ronde.getAfspraken().isEmpty())
			{
				keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK);
			}
			keuzes.add(TestVervolgKeuzeOptie.MDL_VERSLAG);
		}
	}

	private void keuzeIntakeAfspraakConclusie(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		var afspraak = ronde.getLaatsteAfspraak();
		if (afspraak != null)
		{
			if (afspraak.getConclusie() == null && !ColonScreeningRondeUtil.heeftAfgerondeVerslag(ronde, VerslagType.MDL))
			{
				keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK_CONCLUSIE);
			}
		}
	}

	@Override
	@Transactional
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (var bsn : model.getBsns())
		{
			clienten.add(maakOfVindClient(model, bsn));
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		var persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeboortedatum(model.getGeboortedatum());
		persoon.setGeslacht(model.getGeslacht());

		var adres = new BagAdres();
		adres.setGbaGemeente(model.getGemeente());

		if (model.getGemeente() != null)
		{
			adres.setGemeenteCode(model.getGemeente().getCode());
			adres.setGemeente(model.getGemeente().getNaam());
		}
		persoon.setGbaAdres(adres);
		return testService.maakClient(persoon);
	}

	@Override
	@Transactional
	public List<Client> maakOfWijzigClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (var bsn : model.getBsns())
		{
			var client = maakOfVindClient(model, bsn);
			clienten.add(client);
			var gbaPersoon = client.getPersoon();
			gbaPersoon.setGeslacht(model.getGeslacht());
			gbaPersoon.setGeboortedatum(model.getGeboortedatum());

			hibernateService.saveOrUpdateAll(gbaPersoon);
		}
		return clienten;
	}

	private int getAantalUitnodigingen(Client client)
	{
		var aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && !screeningRonde.getUitnodigingen().isEmpty())
			{
				aantalUitnodigingen = screeningRonde.getUitnodigingen().size();
			}
		}
		return aantalUitnodigingen;
	}

	private int getAantalTestbuizen(Client client)
	{
		var aantalBuizen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getIfobtTesten() != null && screeningRonde.getIfobtTesten().size() > 0)
			{
				aantalBuizen = screeningRonde.getIfobtTesten().size();
			}
		}
		return aantalBuizen;
	}

	private boolean heeftLaatsteScreeningsRonde(Client client)
	{
		return client.getColonDossier().getLaatsteScreeningRonde() != null;
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{

		if (clienten.size() == 1)
		{
			return new ArrayList<>();
		}
		Client eersteClient = null;
		var aantalUitnodigingen = 0;
		var aantalTestbuizen = 0;
		var errorMeldingen = new ArrayList<String>();
		for (var client : clienten)
		{
			if (eersteClient == null)
			{
				aantalUitnodigingen = getAantalUitnodigingen(client);
				aantalTestbuizen = getAantalTestbuizen(client);
				eersteClient = client;
			}

			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				var error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMeldingen.add(error);
			}
			if (aantalTestbuizen != getAantalTestbuizen(client))
			{
				var error = "Het aantal testbuizen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMeldingen.add(error);
			}
		}
		return errorMeldingen;
	}

	@Override
	@Transactional
	public ColonDossier maakNieuweScreeningRonde(Client client, TestTimeLineDossierTijdstip tijdstip, ColonOnderzoeksVariant onderzoeksVariant)
	{
		if (client.getColonDossier().getLaatsteScreeningRonde() != null)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), TestTimeLineDossierTijdstip.EINDE_RONDE);
		}

		var dossier = client.getColonDossier();
		var ronde = newColonScreeningRonde(dossier);
		geefColonVooraankondiging(ronde);

		if (tijdstip == TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_ZONDER_FIT)
		{
			briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_ZONDER_FIT, currentDateSupplier.getDate());
		}
		else
		{
			if (new ArrayList<>(dossier.getScreeningRondes()).size() == 1)
			{
				newColonUitnodiging(ronde, ColonUitnodigingCategorie.U1, onderzoeksVariant);
			}
			else
			{
				newColonUitnodiging(ronde, ColonUitnodigingCategorie.U2, onderzoeksVariant);
			}

			bewerkUitnodiging(client, tijdstip);
		}
		return dossier;
	}

	@Override
	@Transactional
	public ColonDossier bewerkUitnodiging(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		var dossier = client.getColonDossier();
		var ronde = geefLaatsteColonScreeningRonde(dossier);
		var uitnodiging = geefColonUitnodiging(ronde);

		if (TestTimeLineDossierTijdstip.DAG_UITNODIGING_VERSTUREN.equals(tijdstip) || TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN.equals(tijdstip))
		{
			var aantalDagen = controleerColonUitnodigingVersturen(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging());
			if (aantalDagen > 0)
			{
				testTimelineTimeService.calculateBackwards(dossier, aantalDagen);
			}

			uitnodiging.setVerstuurdDatum(currentDateSupplier.getDate());
			uitnodiging.setVerstuurd(true);
		}

		if (TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN.equals(tijdstip))
		{

			testTimelineTimeService.calculateBackwards(dossier, 1);

			koppelFit(uitnodiging);

		}
		return dossier;
	}

	private int controleerColonUitnodigingVersturen(ColonUitnodiging uitnodiging)
	{
		return (int) ChronoUnit.DAYS.between(currentDateSupplier.getLocalDate(), DateUtil.toLocalDate(uitnodiging.getUitnodigingsDatum()));
	}

	private int bepaalAantalDagenVoorOntvangenAntwoordformulierOfFit(ColonUitnodiging uitnodiging)
	{

		var gold = uitnodiging.getGekoppeldeTest();
		if (gold != null && (gold.getAnalyseDatum() != null || IFOBTTestStatus.isUnmutableEindStatus(gold.getStatus()))
			|| uitnodiging.getAntwoordFormulier() != null && uitnodiging.getAntwoordFormulier().getScanDatum() != null)
		{
			return 0;
		}
		uitnodiging = uitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
		var verstuurdDatum = uitnodiging.getVerstuurdDatum();
		if (verstuurdDatum == null)
		{
			verstuurdDatum = uitnodiging.getCreatieDatum();
		}
		var datumVerstuurd = DateUtil.toLocalDate(verstuurdDatum).plusDays(DEFAULT_TIJD_TUSSEN_VERSTUURD_EN_UITSLAG_ONTVANGEN);
		return (int) ChronoUnit.DAYS.between(datumVerstuurd, currentDateSupplier.getLocalDate());
	}

	private ColonScreeningRonde newColonScreeningRonde(ColonDossier dossier)
	{
		var ronde = dossier.getLaatsteScreeningRonde();
		var nu = currentDateSupplier.getDate();
		if (ronde != null)
		{
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(nu);
		}
		ronde = new ColonScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.LOPEND);
		ronde.setCreatieDatum(nu);
		ronde.setStatusDatum(nu);
		ronde.setAangemeld(true);
		ronde.setDossier(dossier);
		dossier.setLaatsteScreeningRonde(ronde);
		dossier.getScreeningRondes().add(ronde);
		hibernateService.saveOrUpdateAll(dossier, ronde);
		ronde = dossier.getLaatsteScreeningRonde();
		return ronde;
	}

	private ColonScreeningRonde geefLaatsteColonScreeningRonde(ColonDossier dossier)
	{
		var ronde = dossier.getLaatsteScreeningRonde();
		if (ronde == null)
		{
			ronde = newColonScreeningRonde(dossier);
		}
		return ronde;
	}

	private ColonVooraankondiging geefColonVooraankondiging(ColonScreeningRonde ronde)
	{
		var dossier = ronde.getDossier();
		var vooraankondiging = dossier.getColonVooraankondiging();
		if (vooraankondiging == null)
		{
			vooraankondiging = new ColonVooraankondiging();
			vooraankondiging.setClient(dossier.getClient());
			var nu = currentDateSupplier.getDate();
			vooraankondiging.setCreatieDatum(nu);
			dossier.setColonVooraankondiging(vooraankondiging);
			var brief = briefService.maakBvoBrief(ronde, BriefType.COLON_VOORAANKONDIGING);
			vooraankondiging.setBrief(brief);
			hibernateService.saveOrUpdateAll(vooraankondiging);
		}
		return vooraankondiging;
	}

	private void koppelFit(ColonUitnodiging uitnodiging)
	{
		if (uitnodiging != null && uitnodiging.getGekoppeldeTest() == null)
		{
			var baseTestBarcode = FITTestUtil.getFITTestBarcode(uitnodiging.getUitnodigingsId());
			var nu = currentDateSupplier.getDate();
			if (ColonOnderzoeksVariant.isOfType(uitnodiging.getOnderzoeksVariant(), IFOBTType.GOLD))
			{
				var ronde = uitnodiging.getScreeningRonde();
				var test = new IFOBTTest();
				test.setDatumVerstuurd(nu);
				test.setStatus(IFOBTTestStatus.ACTIEF);
				test.setStatusDatum(nu);
				test.setBarcode("TGD" + baseTestBarcode);
				test.setColonScreeningRonde(ronde);
				test.setColonUitnodiging(uitnodiging);
				test.setType(IFOBTType.GOLD);
				ronde.getIfobtTesten().add(test);
				ronde.setLaatsteIFOBTTest(test);
				ronde.setLaatsteIFOBTTestExtra(null);
				test.setColonUitnodiging(uitnodiging);
				uitnodiging.setGekoppeldeTest(test);
				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				hibernateService.saveOrUpdateAll(test, uitnodiging, ronde);
			}
			if (ColonOnderzoeksVariant.isOfType(uitnodiging.getOnderzoeksVariant(), IFOBTType.STUDIE))
			{
				var ronde = uitnodiging.getScreeningRonde();
				var test = new IFOBTTest();
				test.setDatumVerstuurd(nu);
				test.setStatus(IFOBTTestStatus.ACTIEF);
				test.setStatusDatum(nu);
				test.setBarcode("TST" + baseTestBarcode);
				test.setColonScreeningRonde(ronde);
				test.setColonUitnodiging(uitnodiging);
				test.setType(IFOBTType.STUDIE);
				ronde.getIfobtTesten().add(test);
				ronde.setLaatsteIFOBTTestExtra(test);
				test.setColonUitnodiging(uitnodiging);
				uitnodiging.setGekoppeldeExtraTest(test);
				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				hibernateService.saveOrUpdateAll(test, uitnodiging, ronde);
			}
		}
	}

	private ColonUitnodiging newColonUitnodiging(ColonScreeningRonde ronde, ColonUitnodigingCategorie cat, ColonOnderzoeksVariant onderzoeksVariant)
	{
		var colonUitnodiging = new ColonUitnodiging();
		colonUitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		if (ColonUitnodigingCategorie.U1.equals(cat) || ColonUitnodigingCategorie.U2.equals(cat))
		{
			colonUitnodiging.setColonUitnodigingCategorie(cat);
			colonUitnodiging.setUitnodigingsDatum(testTimelineTimeService.getVooraankondigingsPeriodeDatum());
		}
		else
		{
			colonUitnodiging.setUitnodigingsDatum(currentDateSupplier.getDate());
		}
		colonUitnodiging.setCreatieDatum(currentDateSupplier.getDate());
		colonUitnodiging.setScreeningRonde(ronde);
		colonUitnodiging.setOnderzoeksVariant(onderzoeksVariant);
		ronde.getUitnodigingen().add(colonUitnodiging);
		ronde.setLaatsteUitnodiging(colonUitnodiging);
		ronde.setLaatsteIFOBTTest(null);
		ronde.setLaatsteIFOBTTestExtra(null);
		hibernateService.saveOrUpdateAll(colonUitnodiging, ronde);
		colonDossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.UITNODIGING_ONTVANGEN);
		colonUitnodiging = ronde.getLaatsteUitnodiging();
		return colonUitnodiging;
	}

	private ColonUitnodiging geefColonUitnodiging(ColonScreeningRonde ronde)
	{
		var uitnodiging = ronde.getLaatsteUitnodiging();
		if (uitnodiging == null)
		{
			if (ronde.getDossier().getScreeningRondes().size() == 1)
			{
				uitnodiging = newColonUitnodiging(ronde, ColonUitnodigingCategorie.U1, ColonOnderzoeksVariant.STANDAARD);
			}
			else
			{
				uitnodiging = newColonUitnodiging(ronde, ColonUitnodigingCategorie.U1, ColonOnderzoeksVariant.STANDAARD);
			}
		}
		return uitnodiging;
	}

	@Override
	public List<TestTimelineRonde> getTimelineRondes(Client client)
	{
		var onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		onderzoeken.add(Bevolkingsonderzoek.COLON);
		var rondes = dossierService.getScreeningRondeGebeurtenissen(client, new ClientDossierFilter(onderzoeken, false));

		return convertToTimelineRondes(rondes);
	}

	private List<TestTimelineRonde> convertToTimelineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<>();
		for (var ronde : rondeDossier)
		{
			var index = ronde.getRondenr() - 1;
			TestTimelineRonde testTimelineRonde;
			if (!rondes.containsKey(index))
			{
				testTimelineRonde = new TestTimelineRonde();
				testTimelineRonde.setRondeNummer(index + 1);
				rondes.put(index, testTimelineRonde);
			}
			else
			{
				testTimelineRonde = rondes.get(index);
			}
			testTimelineRonde.setColonScreeningRondeDossier(ronde);
		}
		return convertHashMapToList(rondes);
	}

	private List<TestTimelineRonde> convertHashMapToList(Map<Integer, TestTimelineRonde> map)
	{
		List<TestTimelineRonde> rondes = new ArrayList<>();
		for (var ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	@Transactional
	public void retourzendingOntvangen(ColonUitnodiging uitnodiging, String reden)
	{
		var dossier = uitnodiging.getScreeningRonde().getDossier();
		testTimelineTimeService.calculateBackwards(dossier, 5);

		retourzendingService.verwerkRetourzendingHandmatig(null, uitnodiging, reden);
	}

	@Override
	@Transactional
	public IFOBTTest fitOntvangen(Client client, Boolean verlopen, IFOBTTest buis, int analyseDatumDiff)
	{

		var uitnodiging = FITTestUtil.getUitnodiging(buis);
		var aantalDagen = bepaalAantalDagenVoorOntvangenAntwoordformulierOfFit(uitnodiging);
		if (aantalDagen >= 0)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), aantalDagen);
		}

		var vandaag = currentDateSupplier.getLocalDate();

		if (FITTestUtil.heeftUitslag(buis))
		{
			var fitVervaldatum = houdbaarheidService.getFitHoudbaarheidVoor(buis.getBarcode());
			IFOBTVervaldatum tijdelijkVervaldatum = null;
			if (fitVervaldatum == null && !Boolean.TRUE.equals(verlopen))
			{
				tijdelijkVervaldatum = new IFOBTVervaldatum();
				tijdelijkVervaldatum.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				tijdelijkVervaldatum.setType(buis.getType());
				tijdelijkVervaldatum.setLengthBarcode(buis.getBarcode().length());
				tijdelijkVervaldatum.setBarcodeStart(buis.getBarcode());
				tijdelijkVervaldatum.setBarcodeEnd(buis.getBarcode());
				hibernateService.saveOrUpdate(tijdelijkVervaldatum);
			}
			else if (fitVervaldatum != null && (buis.getBarcode().startsWith("TGD") || buis.getBarcode().startsWith("TST")))
			{
				fitVervaldatum.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				hibernateService.saveOrUpdate(fitVervaldatum);
			}
			buis.setAnalyseDatum(DateUtil.toUtilDate(vandaag.minusDays(analyseDatumDiff)));
			buis.setVerwerkingsDatum(DateUtil.toUtilDate(vandaag));
			uitslagOntvangen(buis);

			if (tijdelijkVervaldatum != null && (buis.getBarcode().startsWith("TGD") || buis.getBarcode().startsWith("TST"))
				&& (IFOBTTestStatus.isUnmutableEindStatus(buis.getStatus()) || buis.getStatus() == IFOBTTestStatus.UITGEVOERD))
			{
				hibernateService.delete(tijdelijkVervaldatum);
			}
		}
		return buis;
	}

	private void uitslagOntvangen(IFOBTTest buis)
	{
		if (buis.getType() != IFOBTType.STUDIE)
		{
			fitService.uitslagFitOntvangen(buis);
		}
		else
		{
			studietestService.verwerkUitslag(buis);
		}
	}

	@Override
	@Transactional
	public ColonScreeningRonde naarEindeVanRonde(Client client)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.EINDE_RONDE);
		return dossier.getLaatsteScreeningRonde();
	}

	@Override
	@Transactional
	public void fitHerinneringVersturen(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, tijdstip);
		if (TestTimeLineDossierTijdstip.DAG_NA_HERINNERING_VERSTUREN.equals(tijdstip))
		{
			var csr = dossier.getLaatsteScreeningRonde();
			var herinneringsBrief = briefService.maakBvoBrief(csr, BriefType.COLON_HERINNERING);
			for (var iTest : rappelerendeFITs(csr))
			{
				iTest.setHerinnering(Boolean.TRUE);
				hibernateService.saveOrUpdate(iTest);
				herinneringsBrief.setIfobtTest(iTest);
				hibernateService.saveOrUpdate(herinneringsBrief);
			}
		}
	}

	private List<IFOBTTest> rappelerendeFITs(ColonScreeningRonde csr)
	{
		return csr.getIfobtTesten().stream().filter(fit -> IFOBTTestStatus.ACTIEF.equals(fit.getStatus()) && fit.getStatusDatum().before(getRapelDate()))
			.collect(Collectors.toList());
	}

	private Date getRapelDate()
	{
		var rapelPeriode = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
		return DateUtil.minDagen(currentDateSupplier.getDate(), rapelPeriode);
	}

	@Override
	@Transactional
	public void verzetDossierAchteruitInTijd(Client client, int aantaldagen)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, aantaldagen);
	}

	@Override
	@Transactional
	public void maaktIntakeAfspraakVoorClient(Client client, ColonIntakelocatie intakelocatie)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(client.getColonDossier(), 1);

		var ronde = dossier.getLaatsteScreeningRonde();

		var brief = briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_INTAKE);
		brief.setCreatieDatum(DateUtil.minusTijdseenheid(brief.getCreatieDatum(), 5, ChronoUnit.SECONDS));

		ColonIntakekamer actieveKamer = null;

		Integer duurAfspraakInMinuten = null;
		for (var kamer : intakelocatie.getKamers())
		{
			if (kamer.getActief())
			{
				actieveKamer = kamer;
				duurAfspraakInMinuten = organisatieParameterService.getOrganisatieParameter(actieveKamer.getIntakelocatie(),
					OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, 15);
				break;
			}
		}

		var intakeAfspraak = ronde.getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			intakeAfspraak = new ColonIntakeAfspraak();
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setClient(client);
			intakeAfspraak.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
			intakeAfspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
			client.getAfspraken().add(intakeAfspraak);
			ronde.setLaatsteAfspraak(intakeAfspraak);
			ronde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setColonScreeningRonde(ronde);
			intakeAfspraak.setVanaf(currentDateSupplier.getLocalDateTime().plusDays(1));
			intakeAfspraak.setKamer(actieveKamer);
			intakeAfspraak.setTot(intakeAfspraak.getVanaf().plusMinutes(duurAfspraakInMinuten));
		}
		intakeAfspraak.setBezwaar(false);
		intakeAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);
		brief.setIntakeAfspraak(intakeAfspraak);
		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(intakeAfspraak);
		hibernateService.saveOrUpdate(ronde);
		hibernateService.saveOrUpdate(client);

		colonDossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

	}

	@Override
	@Transactional
	public void maakIntakeAfspraakConclusieVoorClient(Client client, ColonConclusieType type)
	{
		var keuzes = new ColonVervolgonderzoekKeuzesDto();
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.INTAKE_AFSPRAAK_CONCLUSIE);
		var ronde = dossier.getLaatsteScreeningRonde();
		var intakeAfspraak = ronde.getLaatsteAfspraak();
		var conclusie = intakeAfspraak.getConclusie();
		var ingelogdeGebruiker = hibernateService.loadAll(InstellingGebruiker.class).get(0);
		keuzes.conclusie = type;
		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
		}

		conclusie.setType(type);
		switch (type)
		{
		case COLOSCOPIE:
			keuzes.intakeConclusie = true;
			conclusie.setDatumColoscopie(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(10)));
		case CT_COLOGRAFIE:
			keuzes.intakeConclusie = true;
			break;
		case NO_SHOW:
		case ON_HOLD:
			keuzes.intakeConclusie = false;
			break;
		case DOORVERWIJZEN_NAAR_ANDER_CENTRUM:
			keuzes.intakeConclusie = false;
			keuzes.verwijzing = true;
			conclusie.setDoorverwijzingBevestigd(true);
			break;
		case CLIENT_WIL_ANDERE_INTAKELOKATIE:
			keuzes.intakeConclusie = false;
			keuzes.verwijzing = false;
			break;
		case GEEN_VERVOLGONDERZOEK:
			keuzes.intakeConclusie = true;
			keuzes.redenGeenVervolgOnderzoek = false;
			break;
		}
		intakeAfspraak.setConclusie(conclusie);

		conclusie.setInstellingGebruiker(ingelogdeGebruiker);

		colonDossierService.conclusieOpslaan(intakeAfspraak, keuzes, ingelogdeGebruiker, null);
	}

	@Override
	@Transactional
	public void maaktMdlVerslagVoorClient(Client client, ColoscopieLocatie locatie, MdlVervolgbeleid vervolgbeleid, Date datumOnderzoek)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.MDL_VERSLAG);
		var ronde = dossier.getLaatsteScreeningRonde();

		var verslag = new MdlVerslag();
		verslag.setDatumOnderzoek(datumOnderzoek);
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verslag.setInvoerder(locatie.getOrganisatieMedewerkers().get(0));
		verslag.setScreeningRonde(ronde);
		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(VerslagType.MDL);
		verslag.setUitvoerderMedewerker(locatie.getOrganisatieMedewerkers().get(0).getMedewerker());
		verslag.setUitvoerderOrganisatie(locatie);
		verslag.setVervolgbeleid(vervolgbeleid);
		var content = new MdlVerslagContent();
		verslag.setVerslagContent(content);
		if (vervolgbeleid != null)
		{
			var coloscopieMedischeObservatie = new MdlColoscopieMedischeObservatie();
			content.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
			coloscopieMedischeObservatie.setVerslagContent(content);
			var definitiefVervolgbeleidVoorBevolkingsonderzoekg = new MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
			coloscopieMedischeObservatie.setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(definitiefVervolgbeleidVoorBevolkingsonderzoekg);

			definitiefVervolgbeleidVoorBevolkingsonderzoekg
				.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagService.getDsValue(vervolgbeleid.getCode(), vervolgbeleid.getCodeSystem(), "vs_vervolgbeleid"));
			definitiefVervolgbeleidVoorBevolkingsonderzoekg.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
		}
		content.setVerslag(verslag);
		ronde.getVerslagen().add(verslag);
		hibernateService.saveOrUpdate(verslag);
		hibernateService.saveOrUpdate(ronde);
		verwerkVerslagService.verwerkInDossier(verslag);
		colonDossierBaseService.setVolgendeUitnodigingVoorVerslag(verslag);
	}
}
