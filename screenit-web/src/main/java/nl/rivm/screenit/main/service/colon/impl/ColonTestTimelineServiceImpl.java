package nl.rivm.screenit.main.service.colon.impl;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
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
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.planning.model.Discipline;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
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
	private IFobtService ifobtService;

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
	private AfspraakDefinitieDao afspraakDefinitieDao;

	@Autowired
	private BaseHoudbaarheidDao houdbaarheidDao;

	@Autowired
	private RetourzendingService retourzendingService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private ColonVerwerkVerslagService verwerkVerslagService;

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private ColonDossierBaseService colonDossierBaseService;

	@Autowired
	private ColonDossierService colonDossierService;

	@Autowired
	private ColonUitnodigingService colonUitnodigingService;

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();
		if (client.getColonDossier().getLaatsteScreeningRonde() != null
			&& client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{
			for (ColonScreeningRonde ronde : client.getColonDossier().getScreeningRondes())
			{
				if (ronde.getLaatsteUitnodiging() != null)
				{
					keuzeAntwoordFormulierEnIfobt(keuzes, ronde);
					keuzeIntakeAfspraak(keuzes, ronde);
					keuzeIntakeAfspraakConclusie(keuzes, ronde);
					keuzeHerinneringIfobt(keuzes, ronde);
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

	private void keuzeHerinneringIfobt(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.HERINNERING))
		{
			for (IFOBTTest test : ronde.getIfobtTesten())
			{
				if (!test.isHerinnering() && test.getStatus().equals(IFOBTTestStatus.ACTIEF))
				{
					keuzes.add(TestVervolgKeuzeOptie.HERINNERING);
					return;
				}
			}
		}
	}

	private void keuzeAntwoordFormulierEnIfobt(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.IFOBT))
		{
			for (IFOBTTest test : ronde.getIfobtTesten())
			{
				{
					if (test.getUitslag() == null)
					{
						keuzes.add(TestVervolgKeuzeOptie.IFOBT);
						return;
					}
				}
			}
		}
	}

	private void keuzeRetourZending(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.RETOURZENDING))
		{
			for (IFOBTTest test : ronde.getIfobtTesten())
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
		ColonIntakeAfspraak afspraak = ronde.getLaatsteAfspraak();
		if (afspraak != null)
		{
			if (afspraak.getConclusie() == null && !ColonScreeningRondeUtil.heeftAfgerondeVerslag(ronde, VerslagType.MDL))
			{
				keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK_CONCLUSIE);
			}
		}
	}

	@Override
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (String bsn : model.getBsns())
		{
			clienten.add(maakOfVindClient(model, bsn));
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		GbaPersoon persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeboortedatum(model.getGeboortedatum());
		persoon.setGeslacht(model.getGeslacht());

		BagAdres adres = new BagAdres();
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
	public List<Client> maakOfWijzigClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (String bsn : model.getBsns())
		{
			Client client = maakOfVindClient(model, bsn);
			clienten.add(client);
			GbaPersoon gbaPersoon = client.getPersoon();
			gbaPersoon.setGeslacht(model.getGeslacht());
			gbaPersoon.setGeboortedatum(model.getGeboortedatum());

			hibernateService.saveOrUpdateAll(gbaPersoon);
		}
		return clienten;
	}

	private int getAantalUitnodigingen(Client client)
	{
		int aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			ColonScreeningRonde screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && !screeningRonde.getUitnodigingen().isEmpty())
			{
				aantalUitnodigingen = screeningRonde.getUitnodigingen().size();
			}
		}
		return aantalUitnodigingen;
	}

	private int getAantalTestbuizen(Client client)
	{
		int aantalBuizen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			ColonScreeningRonde screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
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
		int aantalUitnodigingen = 0;
		int aantalTestbuizen = 0;
		List<String> errorMelding = new ArrayList<>();
		for (Client client : clienten)
		{
			if (eersteClient == null)
			{
				aantalUitnodigingen = getAantalUitnodigingen(client);
				aantalTestbuizen = getAantalTestbuizen(client);
				eersteClient = client;
			}

			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				String error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMelding.add(error);
			}
			if (aantalTestbuizen != getAantalTestbuizen(client))
			{
				String error = "Het aantal testbuizen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMelding.add(error);
			}
		}
		return errorMelding;
	}

	@Override
	public ColonDossier maakNieuweScreeningRonde(Client client, TestTimeLineDossierTijdstip tijdstip, ColonOnderzoeksVariant onderzoeksVariant)
	{
		if (client.getColonDossier().getLaatsteScreeningRonde() != null)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), TestTimeLineDossierTijdstip.EINDE_RONDE);
		}

		ColonDossier dossier = client.getColonDossier();
		ColonScreeningRonde ronde = newColonScreeningRonde(dossier);
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
	public ColonDossier bewerkUitnodiging(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		ColonDossier dossier = client.getColonDossier();
		ColonScreeningRonde ronde = geefLaatsteColonScreeningRonde(dossier);
		ColonUitnodiging uitnodiging = geefColonUitnodiging(ronde);

		if (TestTimeLineDossierTijdstip.DAG_UITNODIGING_VERSTUREN.equals(tijdstip) || TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN.equals(tijdstip))
		{
			int aantalDagen = controleerColonUitnodigingVersturen(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging());
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

			koppelIFOBTTest(uitnodiging);

		}
		return dossier;
	}

	private int controleerColonUitnodigingVersturen(ColonUitnodiging uitnodiging)
	{
		return (int) ChronoUnit.DAYS.between(currentDateSupplier.getLocalDate(), DateUtil.toLocalDate(uitnodiging.getUitnodigingsDatum()));
	}

	private int bepaalAantalDagenVoorOnvangenAntwoordformulierOfIfobtTest(ColonUitnodiging uitnodiging)
	{

		IFOBTTest gold = uitnodiging.getGekoppeldeTest();
		if (gold != null && (gold.getAnalyseDatum() != null || IFOBTTestStatus.isUnmutableEindStatus(gold.getStatus()))
			|| uitnodiging.getAntwoordFormulier() != null && uitnodiging.getAntwoordFormulier().getScanDatum() != null)
		{
			return 0;
		}
		uitnodiging = uitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
		Date verstuurdDatum = uitnodiging.getVerstuurdDatum();
		if (verstuurdDatum == null)
		{
			verstuurdDatum = uitnodiging.getCreatieDatum();
		}
		LocalDate datumVerstuurd = DateUtil.toLocalDate(verstuurdDatum).plusDays(DEFAULT_TIJD_TUSSEN_VERSTUURD_EN_UITSLAG_ONTVANGEN);
		return (int) ChronoUnit.DAYS.between(datumVerstuurd, currentDateSupplier.getLocalDate());
	}

	private ColonScreeningRonde newColonScreeningRonde(ColonDossier dossier)
	{
		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		Date nu = currentDateSupplier.getDate();
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
		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		if (ronde == null)
		{
			ronde = newColonScreeningRonde(dossier);
		}
		return ronde;
	}

	private ColonVooraankondiging geefColonVooraankondiging(ColonScreeningRonde ronde)
	{
		ColonDossier dossier = ronde.getDossier();
		ColonVooraankondiging vooraankondiging = dossier.getColonVooraankondiging();
		if (vooraankondiging == null)
		{
			vooraankondiging = new ColonVooraankondiging();
			vooraankondiging.setClient(dossier.getClient());
			Date nu = currentDateSupplier.getDate();
			vooraankondiging.setCreatieDatum(nu);
			dossier.setColonVooraankondiging(vooraankondiging);
			ColonBrief brief = briefService.maakBvoBrief(ronde, BriefType.COLON_VOORAANKONDIGING);
			vooraankondiging.setBrief(brief);
			hibernateService.saveOrUpdateAll(vooraankondiging);
		}
		return vooraankondiging;
	}

	private void koppelIFOBTTest(ColonUitnodiging uitnodiging)
	{
		if (uitnodiging != null && uitnodiging.getGekoppeldeTest() == null)
		{
			String baseTestBarcode = FITTestUtil.getFITTestBarcode(uitnodiging.getUitnodigingsId());
			Date nu = currentDateSupplier.getDate();
			if (ColonOnderzoeksVariant.isOfType(uitnodiging.getOnderzoeksVariant(), IFOBTType.GOLD))
			{
				ColonScreeningRonde ronde = uitnodiging.getScreeningRonde();
				IFOBTTest test = new IFOBTTest();
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
				ColonScreeningRonde ronde = uitnodiging.getScreeningRonde();
				IFOBTTest test = new IFOBTTest();
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

		ColonUitnodiging colonUitnodiging = new ColonUitnodiging();
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
		ColonUitnodiging uitnodiging = ronde.getLaatsteUitnodiging();
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
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
		onderzoeken.add(Bevolkingsonderzoek.COLON);
		List<ScreeningRondeGebeurtenissen> rondes = dossierService.getScreeningRondeGebeurtenissen(client, new ClientDossierFilter(onderzoeken, false));

		List<TestTimelineRonde> timeLineRondes = convertToTimeLineRondes(rondes);
		return timeLineRondes;
	}

	private List<TestTimelineRonde> convertToTimeLineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<>();
		for (ScreeningRondeGebeurtenissen ronde : rondeDossier)
		{
			Integer index = new Integer(ronde.getRondenr() - 1);
			TestTimelineRonde testTimeLineRonde = null;
			if (!rondes.containsKey(index))
			{
				testTimeLineRonde = new TestTimelineRonde();
				testTimeLineRonde.setRondeNummer(index + 1);
				rondes.put(index, testTimeLineRonde);
			}
			else
			{
				testTimeLineRonde = rondes.get(index);
			}
			testTimeLineRonde.setColonScreeningRondeDossier(ronde);
		}
		return convertHashMapToList(rondes);
	}

	private List<TestTimelineRonde> convertHashMapToList(Map<Integer, TestTimelineRonde> map)
	{
		List<TestTimelineRonde> rondes = new ArrayList<>();
		for (Entry<Integer, TestTimelineRonde> ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	public void retourzendingOntvangen(ColonUitnodiging uitnodiging, String reden)
	{
		ColonDossier dossier = uitnodiging.getScreeningRonde().getDossier();
		testTimelineTimeService.calculateBackwards(dossier, 5);

		retourzendingService.verwerkRetourzendingHandmatig(null, uitnodiging, reden);
	}

	@Override
	public IFOBTTest ifobtTestOntvangen(Client client, Boolean verlopen, IFOBTTest buis, int analyseDatumDiff)
	{

		ColonUitnodiging uitnodiging = FITTestUtil.getUitnodiging(buis);
		int aantalDagen = bepaalAantalDagenVoorOnvangenAntwoordformulierOfIfobtTest(uitnodiging);
		if (aantalDagen >= 0)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), aantalDagen);
		}

		var vandaag = currentDateSupplier.getLocalDate();

		if (FITTestUtil.heeftUitslag(buis))
		{
			IFOBTVervaldatum ifobtVervaldatum = houdbaarheidDao.getHoudbaarheidVoor(IFOBTVervaldatum.class, buis.getBarcode());
			IFOBTVervaldatum tijdelijkVervaldatum = null;
			if (ifobtVervaldatum == null && !Boolean.TRUE.equals(verlopen))
			{
				tijdelijkVervaldatum = new IFOBTVervaldatum();
				tijdelijkVervaldatum.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				tijdelijkVervaldatum.setType(buis.getType());
				tijdelijkVervaldatum.setLengthBarcode(buis.getBarcode().length());
				tijdelijkVervaldatum.setBarcodeStart(buis.getBarcode());
				tijdelijkVervaldatum.setBarcodeEnd(buis.getBarcode());
				hibernateService.saveOrUpdate(tijdelijkVervaldatum);
			}
			else if (ifobtVervaldatum != null && (buis.getBarcode().startsWith("TGD") || buis.getBarcode().startsWith("TST")))
			{
				ifobtVervaldatum.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				hibernateService.saveOrUpdate(ifobtVervaldatum);
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
			ifobtService.uitslagFitOntvangen(buis);
		}
		else
		{
			studietestService.verwerkUitslag(buis);
		}
	}

	@Override
	public ColonScreeningRonde naarEindeVanRonde(Client client)
	{
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.EINDE_RONDE);
		return dossier.getLaatsteScreeningRonde();
	}

	@Override
	public void ifobtHerinneringVersturen(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, tijdstip);
		if (TestTimeLineDossierTijdstip.DAG_NA_HERINNERING_VERSTUREN.equals(tijdstip))
		{
			ColonScreeningRonde csr = dossier.getLaatsteScreeningRonde();
			ColonBrief herinneringsBrief = briefService.maakBvoBrief(csr, BriefType.COLON_HERINNERING);
			for (IFOBTTest iTest : rappelerendeIfobts(csr))
			{
				iTest.setHerinnering(Boolean.TRUE);
				hibernateService.saveOrUpdate(iTest);
				herinneringsBrief.setIfobtTest(iTest);
				hibernateService.saveOrUpdate(herinneringsBrief);
			}
		}
	}

	private List<IFOBTTest> rappelerendeIfobts(ColonScreeningRonde csr)
	{
		List<IFOBTTest> rappeleren = new ArrayList<>();
		Date rapelDate = getRapelDate();
		for (IFOBTTest test : csr.getIfobtTesten())
		{
			if (IFOBTTestStatus.ACTIEF.equals(test.getStatus()) && test.getStatusDatum().before(rapelDate))
			{
				rappeleren.add(test);
			}
		}

		return rappeleren;
	}

	private Date getRapelDate()
	{
		var rapelPeriode = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
		return DateUtil.minDagen(currentDateSupplier.getDate(), rapelPeriode);
	}

	@Override
	public void verzetDossierAchteruitInTijd(Client client, int aantaldagen)
	{
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, aantaldagen);
	}

	@Override
	public void maaktIntakeAfspraakVoorClient(Client client, ColoscopieCentrum centrum)
	{
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(client.getColonDossier(), 1);

		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();

		ColonBrief brief = briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_INTAKE);
		brief.setCreatieDatum(DateUtil.minusTijdseenheid(brief.getCreatieDatum(), 5, ChronoUnit.SECONDS));

		ColonIntakeAfspraak intakeAfspraak = ronde.getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			intakeAfspraak = new ColonIntakeAfspraak();
			intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setAfspraaknummer(System.currentTimeMillis());
			intakeAfspraak.setClient(client);
			intakeAfspraak.setBezwaar(Boolean.FALSE);
			intakeAfspraak.setDatumLaatsteWijziging(currentDateSupplier.getDate());
			client.getAfspraken().add(intakeAfspraak);
			ronde.setLaatsteAfspraak(intakeAfspraak);
			ronde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setColonScreeningRonde(ronde);
			intakeAfspraak.getDisciplines().add(hibernateService.loadAll(Discipline.class).get(0));
		}
		intakeAfspraak.setBezwaar(Boolean.FALSE);
		intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
		if (intakeAfspraak.getStartTime() == null)
		{
			intakeAfspraak.setStartTime(DateUtil.plusDagen(currentDateSupplier.getDate(), 1));
		}
		if (intakeAfspraak.getLocation() == null)
		{
			Kamer actieveKamer = null;
			for (Kamer kamer : centrum.getKamers())
			{
				if (kamer.getActief())
				{
					actieveKamer = kamer;
					break;
				}
			}
			intakeAfspraak.setLocation(actieveKamer);
		}
		if (intakeAfspraak.getDefinition() == null)
		{
			List<AfspraakDefinitie> afspraakDefinities = afspraakDefinitieDao.getActieveActieDefinities(intakeAfspraak.getLocation().getColoscopieCentrum());
			intakeAfspraak.setDefinition(afspraakDefinities.get(0));
		}
		if (intakeAfspraak.getEndTime() == null)
		{
			intakeAfspraak.setEndTime(DateUtil.plusTijdseenheid(intakeAfspraak.getStartTime(), intakeAfspraak.getDefinition().getDuurAfspraakInMinuten(), ChronoUnit.MINUTES));
		}
		brief.setIntakeAfspraak(intakeAfspraak);
		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(intakeAfspraak);
		hibernateService.saveOrUpdate(ronde);
		hibernateService.saveOrUpdate(client);

		colonDossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

	}

	@Override
	public void maakIntakeAfspraakConclusieVoorClient(Client client, ColonConclusieType type)
	{
		ColonVervolgonderzoekKeuzesDto keuzes = new ColonVervolgonderzoekKeuzesDto();
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.INTAKE_AFSPRAAK_CONCLUSIE);
		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		ColonIntakeAfspraak intakeAfspraak = ronde.getLaatsteAfspraak();
		ColonConclusie conclusie = intakeAfspraak.getConclusie();
		InstellingGebruiker ingelogdeGebruiker = hibernateService.loadAll(InstellingGebruiker.class).get(0);
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
	public void maaktMdlVerslagVoorClient(Client client, ColoscopieLocatie locatie, MdlVervolgbeleid vervolgbeleid, Date datumOnderzoek)
	{
		ColonDossier dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.MDL_VERSLAG);
		ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();

		MdlVerslag verslag = new MdlVerslag();
		verslag.setDatumOnderzoek(datumOnderzoek);
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verslag.setInvoerder(locatie.getOrganisatieMedewerkers().get(0));
		verslag.setScreeningRonde(ronde);
		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(VerslagType.MDL);
		verslag.setUitvoerderMedewerker(locatie.getOrganisatieMedewerkers().get(0).getMedewerker());
		verslag.setUitvoerderOrganisatie(locatie);
		verslag.setVervolgbeleid(vervolgbeleid);
		MdlVerslagContent content = new MdlVerslagContent();
		verslag.setVerslagContent(content);
		if (vervolgbeleid != null)
		{
			MdlColoscopieMedischeObservatie coloscopieMedischeObservatie = new MdlColoscopieMedischeObservatie();
			content.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
			coloscopieMedischeObservatie.setVerslagContent(content);
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidVoorBevolkingsonderzoekg = new MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
			coloscopieMedischeObservatie.setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(definitiefVervolgbeleidVoorBevolkingsonderzoekg);

			definitiefVervolgbeleidVoorBevolkingsonderzoekg
				.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagDao.getDsValue(vervolgbeleid.getCode(), vervolgbeleid.getCodeSystem(), "vs_vervolgbeleid"));
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
