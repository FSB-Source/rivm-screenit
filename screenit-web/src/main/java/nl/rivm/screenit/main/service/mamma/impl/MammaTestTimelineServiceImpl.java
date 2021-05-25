package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.ImportPocOpties;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDeelnamekans;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOpkomstkans;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaSignaleren;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.rivm.screenit.service.mamma.enums.MammaTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.util.bsn.BsnUtils;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Charsets;

import au.com.bytecode.opencsv.CSVParser;
import au.com.bytecode.opencsv.CSVReader;
import ca.uhn.hl7v2.HL7Exception;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaTestTimelineServiceImpl implements MammaTestTimelineService
{

	private static final Logger LOG = LoggerFactory.getLogger(MammaTestTimelineServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private DossierService dossierService;

	@Autowired
	private MammaBaseTestService baseTestService;

	@Autowired
	private MammaBaseTestTimelineTimeService baseTestTimelineTimeService;

	@Autowired
	private MammaBaseTestTimelineService baseTestTimelineService;

	@Autowired
	private MammaBeoordelingService beoordelingService;

	@Autowired
	private MammaBaseOnderzoekService onderzoekService;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private MammaScreeningsEenheidDao screeningsEenheidDao;

	@Autowired
	private MammaStandplaatsService standplaatsService;

	@Autowired
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private EnovationHuisartsService enovationHuisartsService;

	@Autowired
	private MedewerkerService medewerkerService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public String setDeelnamekansen(InputStream inputStream)
	{
		String bsn = null;
		String result = "Succesvol";
		int aantal = 0;
		try (Scanner scanner = new Scanner(inputStream, Charsets.UTF_8.name()))
		{
			CSVParser parser = new CSVParser(';');
			Map<String, Integer> columnMap = new HashMap<>();

			String[] columns = parser.parseLine(scanner.nextLine());
			for (int i = 0; i < columns.length; i++)
			{
				columnMap.put(StringUtils.trim(columns[i]), i);
			}

			while (scanner.hasNextLine())
			{
				columns = parser.parseLine(scanner.nextLine());

				bsn = StringUtils.trim(columns[columnMap.get("Bsn")]);
				if (!BsnUtils.isValidBSN(bsn, true, true))
				{
					throw new IllegalArgumentException("BSN niet valide");
				}
				MammaDoelgroep doelgroep = MammaDoelgroep.valueOf(StringUtils.trim(columns[columnMap.get("Doelgroep")]));
				String postcode = StringUtils.trim(columns[columnMap.get("Postcode")]);
				BigDecimal deelnamekans = new BigDecimal(StringUtils.trim(columns[columnMap.get("Deelnamekans")]));
				BigDecimal opkomstkans = new BigDecimal(StringUtils.trim(columns[columnMap.get("Opkomstkans")]));
				Date geboortedatum = new SimpleDateFormat("dd/MM/yyyy").parse(StringUtils.trim(columns[columnMap.get("geboortedatum")]));

				baseTestService.maakOfVindClient(bsn, doelgroep, postcode, deelnamekans, opkomstkans, geboortedatum);
				aantal++;
			}
		}
		catch (Exception e)
		{
			if (bsn != null)
			{
				result = "Fout bij verwerken van BSN " + bsn;
				LOG.error("error bij bsn " + bsn, e);
			}
			else
			{
				result = "Fout tijdens verwerking van bestand " + e.getMessage();
			}
		}
		return result + ". #" + aantal + " verwerkt.";
	}

	@Override
	public List<TestTimelineRonde> getTimelineRondes(Client client)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		onderzoeken.add(Bevolkingsonderzoek.MAMMA);
		List<ScreeningRondeGebeurtenissen> rondes = dossierService.getScreeningRondeGebeurtenissen(client, new ClientDossierFilter(onderzoeken, false));

		List<TestTimelineRonde> timeLineRondes = convertToTimeLineRondes(rondes);
		return timeLineRondes;
	}

	private List<TestTimelineRonde> convertToTimeLineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<Integer, TestTimelineRonde>();
		Integer index = 0;
		for (ScreeningRondeGebeurtenissen ronde : rondeDossier)
		{
			TestTimelineRonde testTimeLineRonde = null;
			if (!rondes.containsKey(index))
			{
				testTimeLineRonde = new TestTimelineRonde();
				testTimeLineRonde.setRondeNummer(ronde.getRondenr());
				rondes.put(index, testTimeLineRonde);
			}
			else
			{
				testTimeLineRonde = rondes.get(index);
			}
			testTimeLineRonde.setMammaScreeningRondeDossier(ronde);
			index++;
		}
		return convertHashMapToList(rondes);
	}

	private List<TestTimelineRonde> convertHashMapToList(Map<Integer, TestTimelineRonde> map)
	{
		List<TestTimelineRonde> rondes = new ArrayList<TestTimelineRonde>();
		for (Map.Entry<Integer, TestTimelineRonde> ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();

		if (client.getPersoon().getGeslacht() == Geslacht.VROUW)
		{
			MammaDossier dossier = client.getMammaDossier();
			MammaScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null)
			{
				if (magAfspraakMaken(ronde))
				{
					keuzes.add(TestVervolgKeuzeOptie.MAMMA_AFSPRAAK);
				}

				if (magOnderzoekStarten(ronde))
				{
					keuzes.add(TestVervolgKeuzeOptie.MAMMA_ONDERZOEK_STARTEN);
				}
				if (magSeAfronden(ronde))
				{
					keuzes.add(TestVervolgKeuzeOptie.MAMMA_ONDERZOEK_AFRONDEN);
				}
				MammaOnderzoek onderzoek = MammaScreeningRondeUtil.getLaatsteOnderzoek(ronde);
				if (onderzoek != null)
				{
					if (onderzoek.getMeekijkverzoek() == null && !onderzoek.isDoorgevoerd())
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_AD_HOC_MEEKIJKVERZOEK);
					}
					if (magSeDoorvoeren(onderzoek))
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_SE_DOORVOEREN);
					}
					if (magBeeldenBeschikbaarMaken(onderzoek))
					{
						keuzes.add(TestVervolgKeuzeOptie.MAMMA_BEELDEN_ONTVANGEN);
					}
					if (onderzoek.getLaatsteBeoordeling() != null && onderzoek.getMammografie() != null
						&& MammaMammografieIlmStatus.beeldenBeschikbaar(onderzoek.getMammografie().getIlmStatus()))
					{
						MammaBeoordeling beoordeling = onderzoek.getLaatsteBeoordeling();
						if (magEersteLezingKrijgen(ronde))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_EERSTE_LEZING_MAKEN);
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_EERSTE_TWEE_LEZINGEN_MAKEN);
						}
						else if (magTweedeLezingKrijgen(ronde))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_TWEEDE_LEZING_MAKEN);
						}
						else if (magNaarArbitrageEnConclusie(ronde))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_DISCREPANTIE_NAAR_ARBITRAGE);
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_DISCREPANTIE_NAAR_CONCLUSIE);
						}
						else if (magVanArbitrageNaarConclusie(ronde))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_ARBITRAGE_NAAR_CONCLUSIE);
						}
						else if (magVerslagMaken(ronde))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_VERSLAG_LEZING_MAKEN);
						}
						else if (MammaBeoordelingStatus.VERSLAG_GEREED.equals(beoordeling.getStatus()))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_STATUS_NAAR_UITSLAG_ONGUNSTIG);
						}
					}
				}
			}
		}
		return keuzes;
	}

	private boolean magOnderzoekStarten(MammaScreeningRonde ronde)
	{
		MammaAfspraak afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(ronde);
		return afspraak != null && afspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && afspraak.getVanaf().compareTo(currentDateSupplier.getDate()) >= 0
			&& afspraak.getOnderzoek() == null;
	}

	private boolean magAfspraakMaken(MammaScreeningRonde ronde)
	{
		return (ronde.getLaatsteUitnodiging() == null || ronde.getLaatsteUitnodiging().getLaatsteAfspraak() == null) && getStandplaatsPeriode(ronde) != null;
	}

	private boolean magSeAfronden(MammaScreeningRonde ronde)
	{
		MammaAfspraak afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(ronde);
		return afspraak != null && (afspraak.getOnderzoek() == null || MammaOnderzoekStatus.ACTIEF == afspraak.getOnderzoek().getStatus());
	}

	private boolean magSeDoorvoeren(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && !onderzoek.isDoorgevoerd() && MammaOnderzoekStatus.ACTIEF != onderzoek.getStatus();
	}

	private boolean magBeeldenBeschikbaarMaken(MammaOnderzoek onderzoek)
	{
		return onderzoek.getMammografie() != null && MammaMammografieIlmStatus.NIET_BESCHIKBAAR.equals(onderzoek.getMammografie().getIlmStatus());
	}

	private boolean magEersteLezingKrijgen(MammaScreeningRonde ronde)
	{
		return MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getEersteLezing() == null;
	}

	private boolean magTweedeLezingKrijgen(MammaScreeningRonde ronde)
	{
		return MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getEersteLezing() != null &&
			MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getTweedeLezing() == null;
	}

	private boolean magNaarArbitrageEnConclusie(MammaScreeningRonde ronde)
	{
		return MammaBeoordelingStatus.DISCREPANTIE.equals(MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getStatus());
	}

	private boolean magVanArbitrageNaarConclusie(MammaScreeningRonde ronde)
	{
		return MammaBeoordelingStatus.ARBITRAGE.equals(MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getStatus());
	}

	private boolean magVerslagMaken(MammaScreeningRonde ronde)
	{
		return MammaBeoordelingStatus.VERSLAG_MAKEN.equals(MammaScreeningRondeUtil.getLaatsteBeoordeling(ronde).getStatus());
	}

	private MammaStandplaatsPeriode getStandplaatsPeriode(MammaScreeningRonde ronde)
	{
		MammaStandplaatsPeriode standplaatsPeriode = null;
		main: for (MammaStandplaats standplaats : baseStandplaatsService
			.getActieveStandplaatsen(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie()))
		{
			for (MammaStandplaatsRonde standplaatsRonde : standplaats.getStandplaatsRonden())
			{
				for (MammaStandplaatsPeriode periode : standplaatsRonde.getStandplaatsPerioden())
				{
					standplaatsPeriode = periode;
					break main;
				}
			}
		}
		return standplaatsPeriode;
	}

	@Override
	public MammaOnderzoek maakOnderzoekVoorBe(MammaAfspraak afspraak, InstellingGebruiker mbber, MammaScreeningsEenheid se)
	{
		return maakOnderzoekVoorBe(afspraak, mbber, se, true);
	}

	private MammaOnderzoek maakOnderzoekVoorBe(MammaAfspraak afspraak, InstellingGebruiker mbber, MammaScreeningsEenheid se, boolean verstuurHl7Berichten)
	{
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		final Client client = screeningRonde.getDossier().getClient();
		if (onderzoek == null)
		{
			MammaDossier dossier = screeningRonde.getDossier();
			baseTestTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.ONDERZOEK_ONTVANGEN);
			onderzoek = maakOnderzoek(afspraak, se);
			onderzoekAfronden(onderzoek);
			if (verstuurHl7Berichten)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(client, MammaHL7v24ORMBerichtStatus.STARTED);
			}
			MammaMammografie mammografie = maakMammaMammografie(onderzoek, mbber);
			onderzoek.setMammografie(mammografie);
			screeningRonde.setLaatsteOnderzoek(onderzoek);
			hibernateService.saveOrUpdateAll(afspraak, mammografie.getVisueleInspectieAfbeelding(), mammografie,
				screeningRonde, dossier);
		}
		onderzoek.setDoorgevoerd(true);
		MammaMammografie mammografie = onderzoek.getMammografie();
		mammografie.setIlmStatus(MammaMammografieIlmStatus.BESCHIKBAAR);
		mammografie.setIlmStatusDatum(currentDateSupplier.getDate());
		MammaBeoordeling beoordeling = onderzoekService.voegInitieleBeoordelingToe(onderzoek);
		setSignalerenVoorOnderzoek(mbber, onderzoek);

		if (afspraak.getStatus() != MammaAfspraakStatus.BEEINDIGD)
		{
			if (verstuurHl7Berichten)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(client, MammaHL7v24ORMBerichtStatus.COMPLETED);
			}
			afspraak.setStatus(MammaAfspraakStatus.BEEINDIGD);
		}
		hibernateService.saveOrUpdateAll(afspraak, onderzoek, mammografie, beoordeling);

		return onderzoek;

	}

	@Override
	public void rondOnderzoekAf(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, boolean verstuurHl7Berichten, OnvolledigOnderzoekOption onvolledigOnderzoekOption,
		OnderbrokenOnderzoekOption onderbrokenOnderzoekOption)
	{
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		MammaScreeningRonde ronde = afspraak.getUitnodiging().getScreeningRonde();
		MammaDossier dossier = ronde.getDossier();
		if (onderzoek == null)
		{
			onderzoek = onderzoekStarten(afspraak, instellingGebruiker, verstuurHl7Berichten);
		}
		onderzoekAfronden(onderzoek);
		MammaMammografie mammografie = maakMammaMammografie(onderzoek, instellingGebruiker);
		onderzoek.setMammografie(mammografie);
		hibernateService.saveOrUpdateAll(mammografie.getVisueleInspectieAfbeelding(), mammografie, dossier);
		EnovationHuisarts huisarts = enovationHuisartsService.zoekHuisartsen(new EnovationHuisarts(), "achternaam", true, 0, 1).iterator().next();
		ronde.setHuisarts(huisarts);
		ronde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
		if (onderzoek.getOnvolledigOnderzoek() == null && onvolledigOnderzoekOption != null)
		{
			onderzoek.setOnvolledigOnderzoek(onvolledigOnderzoekOption);
			onderzoek.setStatus(MammaOnderzoekStatus.ONVOLLEDIG);
		}
		else if (onderzoek.getOnderbrokenOnderzoek() == null && onderbrokenOnderzoekOption != null)
		{
			onderzoek.setOnderbrokenOnderzoek(onderbrokenOnderzoekOption);
			onderzoek.setStatus(MammaOnderzoekStatus.ONDERBROKEN);
		}
		setSignalerenVoorOnderzoek(instellingGebruiker, onderzoek);

		if (afspraak.getStatus() != MammaAfspraakStatus.BEEINDIGD)
		{
			if (verstuurHl7Berichten)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(client, MammaHL7v24ORMBerichtStatus.COMPLETED);
			}
			afspraak.setStatus(MammaAfspraakStatus.BEEINDIGD);
		}
		hibernateService.saveOrUpdateAll(afspraak, onderzoek);
	}

	private MammaOnderzoek onderzoekStarten(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, boolean verstuurHl7Berichten)
	{
		MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		MammaDossier dossier = screeningRonde.getDossier();
		baseTestTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.ONDERZOEK_ONTVANGEN);
		MammaOnderzoek onderzoek = maakOnderzoek(afspraak, afspraak.getStandplaatsPeriode().getScreeningsEenheid());
		afspraak.setIngeschrevenDoor(instellingGebruiker);
		afspraak.setIngeschrevenOp(currentDateSupplier.getDate());
		onderzoek.setDoorgevoerd(false);
		screeningRonde.setLaatsteOnderzoek(onderzoek);
		if (verstuurHl7Berichten)
		{
			berichtToBatchService.queueMammaHL7v24BerichtUitgaand(dossier.getClient(), MammaHL7v24ORMBerichtStatus.STARTED);
		}
		hibernateService.saveOrUpdateAll(onderzoek, afspraak, screeningRonde, dossier);
		return onderzoek;
	}

	private void setSignalerenVoorOnderzoek(InstellingGebruiker mbber, MammaOnderzoek onderzoek)
	{
		if (onderzoek.getSignaleren() == null)
		{
			MammaSignaleren signaleren = new MammaSignaleren();
			signaleren.setHeeftAfwijkingen(false);
			signaleren.setOnderzoek(onderzoek);
			signaleren.setAfgerondDoor(mbber);
			signaleren.setAfgerondOp(currentDateSupplier.getDate());
			onderzoek.setSignaleren(signaleren);
		}
	}

	private MammaOnderzoek maakOnderzoek(MammaAfspraak afspraak, MammaScreeningsEenheid se)
	{
		MammaOnderzoek onderzoek = new MammaOnderzoek();
		onderzoek.setCreatieDatum(currentDateSupplier.getDate());
		onderzoek.setAfspraak(afspraak);
		onderzoek.setScreeningsEenheid(se);
		onderzoek.setStatus(MammaOnderzoekStatus.ACTIEF);
		onderzoek.setOperatieRechts(Boolean.FALSE);
		onderzoek.setOperatieLinks(Boolean.FALSE);
		onderzoek.setAmputatie(null);
		onderzoek.setDoorgevoerd(false);

		afspraak.setOnderzoek(onderzoek);

		baseKansberekeningService.screeningRondeSampleHerzien(afspraak.getUitnodiging().getScreeningRonde());
		return onderzoek;
	}

	private void onderzoekAfronden(MammaOnderzoek onderzoek)
	{
		onderzoek.setOpmerkingVoorRadioloog("Prothese in rechterborst");
		onderzoek.setOperatieRechts(Boolean.TRUE);
		onderzoek.setOperatieLinks(Boolean.FALSE);
		onderzoek.setAanvullendeInformatieOperatie("Borst rechts verwijderd");
		onderzoek.setStatus(MammaOnderzoekStatus.AFGEROND);
		onderzoek.setAfgerondOp(currentDateSupplier.getDate());
	}

	private MammaMammografie maakMammaMammografie(MammaOnderzoek onderzoek, InstellingGebruiker mbber)
	{
		MammaMammografie mammografie = baseFactory.maakMammografie(onderzoek, mbber, new MammaAnnotatieAfbeelding());

		MammaDossier dossier = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getDossier();
		dossier.setLaatsteMammografieAfgerond(mammografie.getAfgerondOp());
		if (dossier.getEersteMammografieAfgerondStandplaatsRonde() == null)
		{
			dossier.setEersteMammografieAfgerondStandplaatsRonde(onderzoek.getAfspraak().getStandplaatsPeriode().getStandplaatsRonde());
		}

		return mammografie;
	}

	@Override
	public void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker)
	{
		voegLezingToe(beoordeling, lezing, gebruiker, true);
	}

	@Override
	public void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker, boolean verstuurHl7Berichten)
	{
		baseBeoordelingService.slaLezingOpEnVerwerkStatus(beoordeling, lezing, gebruiker,
			(b) -> "Niet opgeschort");
		baseBeoordelingService.bevestigLezing(beoordeling, verstuurHl7Berichten);
	}

	@Override
	public void voegEersteTweeLezingenToe(MammaBeoordeling beoordeling, MammaLezing lezing1, MammaLezing lezing2, InstellingGebruiker instellingGebruiker,
		boolean verstuurHl7Berichten)
	{
		voegLezingToe(beoordeling, lezing1, instellingGebruiker, verstuurHl7Berichten);
		voegLezingToe(beoordeling, lezing2, instellingGebruiker, verstuurHl7Berichten);
	}

	@Override
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (String bsn : model.getBsns())
		{
			Client client = maakOfVindClient(model, bsn);
			clienten.add(client);
		}
		return clienten;
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
			gbaPersoon.setGeboortedatum(model.getGeboortedatum());
			gbaPersoon.getGbaAdres().setPostcode(model.getPostcode());
			MammaDossier dossier = client.getMammaDossier();
			dossier.setDoelgroep(model.getDoelgroep());

			MammaDeelnamekans deelnamekans = dossier.getDeelnamekans();
			deelnamekans.setDeelnamekans(model.getDeelnamekans());

			for (MammaScreeningRonde screeningRonde : dossier.getScreeningRondes())
			{
				for (MammaUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
				{
					for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
					{
						MammaOpkomstkans opkomstkans = afspraak.getOpkomstkans();
						opkomstkans.setOpkomstkans(model.getDeelnamekans());
						hibernateService.saveOrUpdate(opkomstkans);
					}
				}
			}

			hibernateService.saveOrUpdateAll(gbaPersoon, dossier, deelnamekans);
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		GbaPersoon persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeslacht(Geslacht.VROUW);
		persoon.setGeboortedatum(model.getGeboortedatum());

		BagAdres adres = new BagAdres();
		adres.setGbaGemeente(model.getGemeente());
		adres.setPostcode(model.getPostcode());
		persoon.setGbaAdres(adres);
		BigDecimal deelnamekans = model.getDeelnamekans();
		return baseTestService.maakOfVindClient(persoon, model.getDoelgroep(), deelnamekans, deelnamekans, true);
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{
		Client eersteClient = null;
		int aantalUitnodigingen = 0;
		List<String> errorMeldingen = new ArrayList<>();
		for (Client client : clienten)
		{
			if (eersteClient == null)
			{
				eersteClient = client;
				aantalUitnodigingen = getAantalUitnodigingen(client);
			}
			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				String error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				addErrorMelding(error, errorMeldingen);
			}
		}
		return errorMeldingen;
	}

	private void addErrorMelding(String error, List<String> meldingen)
	{
		LOG.error(error);
		meldingen.add(error);
	}

	private int getAantalUitnodigingen(Client client)
	{
		int aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			MammaScreeningRonde screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && screeningRonde.getUitnodigingen().size() > 0)
			{
				aantalUitnodigingen = screeningRonde.getUitnodigingen().size();
			}
		}
		return aantalUitnodigingen;
	}

	private boolean heeftLaatsteScreeningsRonde(Client client)
	{
		return client.getMammaDossier().getLaatsteScreeningRonde() != null;
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenheden(ScreeningOrganisatie screeningOrganisatie)
	{
		return screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(screeningOrganisatie);
	}

	@Override
	public int importPocClienten(File file, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid screeningsEenheid, ImportPocOpties importPocOpties)
	{
		boolean klaarzettenPlanning = importPocOpties.equals(ImportPocOpties.KLAARZETTEN_VOOR_PLANNING);
		int aantal = 0;
		try (CSVReader reader = new CSVReader(new FileReader(file), ';'))
		{
			String[] huidigeLine = reader.readNext(); 
			String[] vorigeLine = null;
			huidigeLine = reader.readNext();
			SimpleDateFormat formatGeboortedatum = new SimpleDateFormat("yyyy-MM-dd");
			while (huidigeLine != null)
			{
				String bsn = huidigeLine[0];
				String naam = huidigeLine[1];
				String gebdatum = huidigeLine[3];
				String uitnodigingsId = huidigeLine[4];
				String postcode = huidigeLine[11];
				String huisnummer = huidigeLine[12];
				String beoordeling = huidigeLine[13];
				GbaPersoon filterPersoon = new GbaPersoon();
				filterPersoon.setBsn(StringUtils.leftPad(bsn, 9, '0'));
				filterPersoon.setGeslacht(Geslacht.VROUW);

				BagAdres gbaAdres = new BagAdres();
				gbaAdres.setPostcode(StringUtils.isNotBlank(postcode) ? postcode : "1234AA");
				gbaAdres.setHuisnummer(StringUtils.isNotBlank(huisnummer) ? Integer.parseInt(huisnummer) : 9);
				gbaAdres.setGbaGemeente(hibernateService.loadAll(Gemeente.class).get(0));
				filterPersoon.setGbaAdres(gbaAdres);
				MammaDossier dossier = baseTestService.geefDossier(filterPersoon);
				MammaDeelnamekans deelnamekans = dossier.getDeelnamekans();
				deelnamekans.setDeelnamekans(BigDecimal.valueOf(0.8));
				hibernateService.saveOrUpdateAll(dossier, deelnamekans);

				GbaPersoon persoon = dossier.getClient().getPersoon();
				String[] naamSplitted = naam.split("\\^");
				persoon.setAchternaam(naamSplitted[0]);
				persoon.setVoornaam(naamSplitted[1]);
				persoon.setVoorletters(null); 
				persoon.setTussenvoegsel(naamSplitted[2]);
				if (naamSplitted.length > 3)
				{
					persoon.setTussenvoegsel(naamSplitted[3]);
				}
				persoon.setGeboortedatum(formatGeboortedatum.parse(gebdatum));
				hibernateService.saveOrUpdate(persoon);

				if (dossier.getLaatsteScreeningRonde() != null && vorigeLine != null && vorigeLine[0].equals(bsn))
				{

					MammaOnderzoek onderzoek = voegPocOnderzoekToe(dossier, screeningsEenheid, instellingGebruiker);
					voegPocBeoordelingToe(onderzoek, instellingGebruiker, vorigeLine[13]);
				}

				MammaStandplaatsRonde standplaatsRonde = null;
				if (klaarzettenPlanning)
				{
					MammaStandplaats standplaats = standplaatsService.getStandplaatsMetPostcode(dossier.getClient());
					standplaatsRonde = hibernateService.loadAll(MammaStandplaatsRonde.class).stream().filter(sr -> !sr.getStandplaats().equals(standplaats))
						.findFirst().orElse(hibernateService.loadAll(MammaStandplaatsRonde.class).get(0));
				}

				boolean rekenDossierTerug = !klaarzettenPlanning;

				MammaUitnodiging uitnodiging = baseTestTimelineService.nieuweRondeAfspraakUitnodiging(dossier.getClient(), screeningsEenheid, standplaatsRonde,
					rekenDossierTerug);
				uitnodiging.getScreeningRonde().setUitnodigingsNr(Long.valueOf(uitnodigingsId));
				hibernateService.saveOrUpdate(uitnodiging);

				if (!importPocOpties.equals(ImportPocOpties.KLAARZETTEN_VOOR_SE))
				{
					MammaOnderzoek onderzoek = voegPocOnderzoekToe(dossier, screeningsEenheid, instellingGebruiker);

					if (klaarzettenPlanning)
					{
						voegPocBeoordelingToe(onderzoek, instellingGebruiker, beoordeling);
					}
				}
				else
				{
					baseTestTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), 35);
				}
				if (klaarzettenPlanning)
				{
					baseTestTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.NIEUWE_RONDE);
				}

				vorigeLine = huidigeLine;
				huidigeLine = reader.readNext();
				aantal++;
			}

		}
		catch (Exception e)
		{
			LOG.error("Fout bij importeren poc clienten", e);
		}
		return aantal;
	}

	private MammaOnderzoek voegPocOnderzoekToe(MammaDossier dossier, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker instellingGebruiker)
	{
		MammaOnderzoek onderzoek = MammaScreeningRondeUtil.getLaatsteOnderzoek(dossier.getLaatsteScreeningRonde());
		if (onderzoek == null)
		{
			onderzoek = maakOnderzoekVoorBe(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak(), instellingGebruiker, screeningsEenheid);
			onderzoek.setDoorgevoerd(true);
			MammaMammografie mammografie = onderzoek.getMammografie();
			mammografie.setAfgerondOp(onderzoek.getAfgerondOp());

			dossier.setLaatsteMammografieAfgerond(mammografie.getAfgerondOp());
			if (dossier.getEersteMammografieAfgerondStandplaatsRonde() == null)
			{
				dossier.setEersteMammografieAfgerondStandplaatsRonde(onderzoek.getAfspraak().getStandplaatsPeriode().getStandplaatsRonde());
			}
			hibernateService.saveOrUpdateAll(mammografie, dossier);
		}
		return onderzoek;
	}

	private void voegPocBeoordelingToe(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker, String beoordelingStatus)
	{
		MammaBeoordeling mammaBeoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(onderzoek);

		if (mammaBeoordeling == null)
		{
			throw new IllegalStateException("Er was geen beoordeling");
		}

		if (mammaBeoordeling.getEersteLezing() == null || mammaBeoordeling.getTweedeLezing() == null)
		{
			InstellingGebruiker radiologen[] = getTweeVerschillendeActieveErvanenRadioloog(mammaBeoordeling.getBeoordelingsEenheid());
			voegPocLezingToe(mammaBeoordeling, beoordelingStatus, radiologen[0]);
			MammaLezing tweedeLezing = voegPocLezingToe(mammaBeoordeling, beoordelingStatus, radiologen[1]);

			if (beoordelingStatus.equals("ONGUNSTIG"))
			{
				MammaLezing verslaglezing = baseBeoordelingService.maakVerslagLezing(mammaBeoordeling, tweedeLezing, instellingGebruiker, false);
				verslaglezing.setBiradsLinks(MammaBIRADSWaarde.VIJF);
				verslaglezing.setBiradsRechts(MammaBIRADSWaarde.VIER);
				voegLezingToe(mammaBeoordeling, verslaglezing, instellingGebruiker);
				verslagGoedkeurenDoorCE(mammaBeoordeling, instellingGebruiker);
			}
			MammaScreeningRonde screeningRonde = mammaBeoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
			screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			screeningRonde.setStatusDatum(onderzoek.getAfgerondOp());

		}
	}

	private MammaLezing voegPocLezingToe(MammaBeoordeling mammaBeoordeling, String beoordelingStatus, InstellingGebruiker beoordelaar)
	{
		MammaLezing lezing = beoordelingService.getOrCreate1eOf2eLezing(mammaBeoordeling, beoordelaar, true);

		if (beoordelingStatus.equals("ONGUNSTIG"))
		{
			lezing.setBiradsLinks(MammaBIRADSWaarde.VIJF);
			lezing.setBiradsRechts(MammaBIRADSWaarde.VIER);
		}
		voegLezingToe(mammaBeoordeling, lezing, beoordelaar);
		return lezing;
	}

	@Override
	public void setUitnodigingsNr(MammaScreeningRonde ronde, Long uitnodigingsNr)
	{
		ronde.setUitnodigingsNr(uitnodigingsNr);
	}

	@Override
	public void verslagGoedkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker)
	{
		beoordelingService.verslagGoedkeurenDoorCE(beoordeling, false, null, ingelogdeGebruiker);
	}

	@Override
	public void doorvoerenOnderzoek(MammaAfspraak afspraak)
	{
		onderzoekService.onderzoekDoorvoerenVanuitSe(afspraak.getOnderzoek());
	}

	@Override
	public void beeldenBeschikbaarBe(MammaAfspraak afspraak)
	{
		try
		{
			MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
			onderzoekService.ontvangBeeldenVoorOnderzoek(screeningRonde.getDossier().getClient(), screeningRonde);
		}
		catch (HL7Exception e)
		{
			LOG.error("Fout bij beelden als ontvangen markeren", e);
		}
	}

	private InstellingGebruiker[] getTweeVerschillendeActieveErvanenRadioloog(BeoordelingsEenheid beoordelingsEenheid)
	{
		InstellingGebruiker zoekInstellingGebruiker = new InstellingGebruiker();
		zoekInstellingGebruiker.setOrganisatie(beoordelingsEenheid);
		List<InstellingGebruiker> instellingGebruikers = medewerkerService.getActieveRadiologen(zoekInstellingGebruiker, new ArrayList<>(), "medewerker.gebruikersnaam", true)
			.stream()
			.filter(m -> beoordelingService.isBevoegdVoorArbitrage(m))
			.collect(Collectors.toList());
		Iterator<InstellingGebruiker> iterator = instellingGebruikers.iterator();

		InstellingGebruiker radiologen[] = new InstellingGebruiker[2];
		radiologen[0] = iterator.next();

		while (iterator.hasNext())
		{
			radiologen[1] = iterator.next();
			if (!radiologen[1].getMedewerker().equals(radiologen[0].getMedewerker()))
			{
				break;
			}
		}

		return radiologen;
	}

	@Override
	public void doorvoerenAdhocMeekijkverzoek(MammaOnderzoek onderzoek)
	{
		baseFactory.maakAdhocMeekijkverzoek(onderzoek, "Op verzoek van SE");
	}

	@Override
	public void doorvoerenOnderzoekStarten(MammaAfspraak afspraak, InstellingGebruiker ingelogdeInstellingGebruiker, boolean verstuurHl7Berichten)
	{
		onderzoekStarten(afspraak, ingelogdeInstellingGebruiker, verstuurHl7Berichten);
	}

	@Override
	public boolean isSnelkeuzeKnopMammaBeschikbaar(Client client, TestTimelineRonde timeLineRonde)
	{
		GbaPersoon persoon = client.getPersoon();
		boolean isOverleden = persoon.getOverlijdensdatum() != null;
		ScreeningRondeGebeurtenissen gebeurtenissen = timeLineRonde.getMammaScreeningRondeDossier();
		MammaScreeningRonde ronde = (MammaScreeningRonde) gebeurtenissen.getScreeningRonde();
		MammaOnderzoek onderzoek = null;
		if (ronde.getLaatsteUitnodiging() != null && ronde.getLaatsteUitnodiging().getLaatsteAfspraak() != null)
		{
			onderzoek = ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
		}
		boolean onderbrokenZonderFotosSitautie = onderzoek != null && magBeeldenBeschikbaarMaken(onderzoek)
			&& MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoek.getStatus()) && OnvolledigOnderzoekOption.ZONDER_FOTOS.equals(onderzoek.getOnvolledigOnderzoek())
			&& ronde.getDossier().getLaatsteScreeningRonde().equals(ronde);
		boolean isLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus());
		boolean isAangemeld = ronde.getAangemeld().booleanValue();
		return !isOverleden && (isLopend || !isAangemeld && !isLopend || onderbrokenZonderFotosSitautie);
	}
}
