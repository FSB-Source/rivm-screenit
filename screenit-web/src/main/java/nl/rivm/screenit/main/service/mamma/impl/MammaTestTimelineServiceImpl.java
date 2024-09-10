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
import java.io.FileReader;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.specification.mamma.MammaMammografieSpecification;
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
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaSignaleren;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.mamma.MammaBaseAfspraakRepository;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.DeelnamemodusDossierService;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.service.mamma.enums.MammaTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.util.bsn.BsnUtils;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVParser;
import au.com.bytecode.opencsv.CSVReader;
import ca.uhn.hl7v2.HL7Exception;

import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.afsprakenWaarvanOnderzoekNietIsDoorgevoerdAfgelopen2Maanden;

@Service
@Slf4j
public class MammaTestTimelineServiceImpl implements MammaTestTimelineService
{
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

	@Autowired
	private DeelnamemodusService deelnamemodusService;

	@Autowired
	private DeelnamemodusDossierService deelnamemodusDossierService;

	@Autowired
	private TransgenderService transgenderService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private ClientRepository clientRepository;

	@Autowired
	private TestService testService;

	@Autowired
	private MammaBaseAfspraakRepository baseAfspraakRepository;

	@Override
	public String setDeelnamekansen(InputStream inputStream)
	{
		String bsn = null;
		var result = "Succesvol";
		var aantal = 0;
		try (var scanner = new Scanner(inputStream, StandardCharsets.UTF_8.name()))
		{
			var parser = new CSVParser(';');
			Map<String, Integer> columnMap = new HashMap<>();

			var columns = parser.parseLine(scanner.nextLine());
			for (var i = 0; i < columns.length; i++)
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
				var doelgroep = MammaDoelgroep.valueOf(StringUtils.trim(columns[columnMap.get("Doelgroep")]));
				var postcode = StringUtils.trim(columns[columnMap.get("Postcode")]);
				var deelnamekans = new BigDecimal(StringUtils.trim(columns[columnMap.get("Deelnamekans")]));
				var opkomstkans = new BigDecimal(StringUtils.trim(columns[columnMap.get("Opkomstkans")]));
				var geboortedatum = new SimpleDateFormat("dd/MM/yyyy").parse(StringUtils.trim(columns[columnMap.get("geboortedatum")]));

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
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
		onderzoeken.add(Bevolkingsonderzoek.MAMMA);
		var rondes = dossierService.getScreeningRondeGebeurtenissen(client, new ClientDossierFilter(onderzoeken, false));

		return convertToTimeLineRondes(rondes);
	}

	private List<TestTimelineRonde> convertToTimeLineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<>();
		Integer index = 0;
		for (var ronde : rondeDossier)
		{
			TestTimelineRonde testTimeLineRonde;
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
		List<TestTimelineRonde> rondes = new ArrayList<>();
		for (var ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();
		var dossier = client.getMammaDossier();
		if (dossier != null)
		{
			var ronde = dossier.getLaatsteScreeningRonde();
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
				var onderzoek = MammaScreeningRondeUtil.getLaatsteOnderzoek(ronde);
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
						var beoordeling = onderzoek.getLaatsteBeoordeling();
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
						else if (MammaBeoordelingStatus.eindStatussen().contains(beoordeling.getStatus()))
						{
							keuzes.add(TestVervolgKeuzeOptie.MAMMA_FOLLOW_UP_VASTLEGGEN);
						}
					}
				}
			}
		}
		return keuzes;
	}

	private boolean magOnderzoekStarten(MammaScreeningRonde ronde)
	{
		var afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(ronde);
		return afspraak != null && afspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && afspraak.getVanaf().compareTo(currentDateSupplier.getDate()) >= 0
			&& afspraak.getOnderzoek() == null;
	}

	private boolean magAfspraakMaken(MammaScreeningRonde ronde)
	{
		return (ronde.getLaatsteUitnodiging() == null || ronde.getLaatsteUitnodiging().getLaatsteAfspraak() == null) && getStandplaatsPeriode(ronde) != null;
	}

	private boolean magSeAfronden(MammaScreeningRonde ronde)
	{
		var afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(ronde);
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
		main:
		for (var standplaats : baseStandplaatsService
			.getActieveStandplaatsen(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie()))
		{
			for (var standplaatsRonde : standplaats.getStandplaatsRonden())
			{
				for (var periode : standplaatsRonde.getStandplaatsPerioden())
				{
					standplaatsPeriode = periode;
					break main;
				}
			}
		}
		return standplaatsPeriode;
	}

	@Override
	@Transactional
	public MammaOnderzoek maakOnderzoekVoorBe(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid se)
	{
		return maakOnderzoekVoorBe(afspraak, instellingGebruiker, se, true);
	}

	private MammaOnderzoek maakOnderzoekVoorBe(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid se, boolean verstuurHl7Berichten)
	{
		var onderzoek = afspraak.getOnderzoek();
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var client = dossier.getClient();
		if (onderzoek == null)
		{
			baseTestTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.ONDERZOEK_ONTVANGEN);
			onderzoek = maakOnderzoek(afspraak, se);
			onderzoekAfronden(onderzoek);
			if (verstuurHl7Berichten)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(client, MammaHL7v24ORMBerichtStatus.STARTED);
			}
			var mammografie = maakMammaMammografie(onderzoek, instellingGebruiker);
			onderzoek.setMammografie(mammografie);
			screeningRonde.setLaatsteOnderzoek(onderzoek);
			hibernateService.saveOrUpdateAll(afspraak, mammografie.getVisueleInspectieAfbeelding(), mammografie,
				screeningRonde, dossier);
		}
		onderzoek.setDoorgevoerd(true);

		var mammografie = onderzoek.getMammografie();
		mammografie.setIlmStatus(MammaMammografieIlmStatus.BESCHIKBAAR);
		mammografie.setIlmStatusDatum(currentDateSupplier.getDate());
		dossier.setLaatsteMammografieAfgerond(mammografie.getAfgerondOp());

		var beoordeling = onderzoekService.voegInitieleBeoordelingToe(onderzoek);
		setSignalerenVoorOnderzoek(instellingGebruiker, onderzoek, false);

		if (afspraak.getStatus() != MammaAfspraakStatus.BEEINDIGD)
		{
			if (verstuurHl7Berichten)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(client, MammaHL7v24ORMBerichtStatus.COMPLETED);
			}
			afspraak.setStatus(MammaAfspraakStatus.BEEINDIGD);
		}
		hibernateService.saveOrUpdateAll(afspraak, onderzoek, mammografie, beoordeling, dossier);

		return onderzoek;

	}

	@Override
	@Transactional
	public void rondOnderzoekAf(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, boolean verstuurHl7Berichten, OnvolledigOnderzoekOption onvolledigOnderzoekOption,
		OnderbrokenOnderzoekOption onderbrokenOnderzoekOption, MammaOnderzoekType onderzoeksType, boolean afwijkingGesignaleerd, MammaDenseWaarde densiteit)
	{
		var onderzoek = afspraak.getOnderzoek();
		var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		var ronde = afspraak.getUitnodiging().getScreeningRonde();
		var dossier = ronde.getDossier();
		if (onderzoek == null)
		{
			onderzoek = onderzoekStarten(afspraak, instellingGebruiker, verstuurHl7Berichten);
		}
		onderzoekAfronden(onderzoek);
		var mammografie = maakMammaMammografie(onderzoek, instellingGebruiker);
		mammografie.setDensiteit(densiteit);
		onderzoek.setMammografie(mammografie);
		hibernateService.saveOrUpdateAll(mammografie.getVisueleInspectieAfbeelding(), mammografie, dossier);
		var huisarts = enovationHuisartsService.zoekHuisartsen(new EnovationHuisarts(), "achternaam", true, 0, 1).iterator().next();
		ronde.setHuisarts(huisarts);
		ronde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
		verwerkOnvolledigOfOnderbrokenOnderzoek(onvolledigOnderzoekOption, onderbrokenOnderzoekOption, onderzoek);
		onderzoek.setOnderzoekType(onderzoeksType);
		setSignalerenVoorOnderzoek(instellingGebruiker, onderzoek, afwijkingGesignaleerd);

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

	private static void verwerkOnvolledigOfOnderbrokenOnderzoek(OnvolledigOnderzoekOption onvolledigOnderzoekOption, OnderbrokenOnderzoekOption onderbrokenOnderzoekOption,
		MammaOnderzoek onderzoek)
	{
		if (onderzoek.getOnvolledigOnderzoek() != null || onvolledigOnderzoekOption != null)
		{
			onderzoek.setStatus(MammaOnderzoekStatus.ONVOLLEDIG);
			if (onderzoek.getOnvolledigOnderzoek() == null)
			{
				onderzoek.setOnvolledigOnderzoek(onvolledigOnderzoekOption);
			}
		}
		if (onderzoek.getOnderbrokenOnderzoek() != null || onderbrokenOnderzoekOption != null)
		{
			onderzoek.setStatus(MammaOnderzoekStatus.ONDERBROKEN);
			if (onderzoek.getOnderbrokenOnderzoek() == null)
			{
				onderzoek.setOnderbrokenOnderzoek(onderbrokenOnderzoekOption);
			}
		}
	}

	private MammaOnderzoek onderzoekStarten(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, boolean verstuurHl7Berichten)
	{
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var dossier = screeningRonde.getDossier();
		baseTestTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.ONDERZOEK_ONTVANGEN);
		var onderzoek = maakOnderzoek(afspraak, afspraak.getStandplaatsPeriode().getScreeningsEenheid());
		afspraak.setIngeschrevenDoor(instellingGebruiker);
		afspraak.setIngeschrevenOp(currentDateSupplier.getDate());
		afspraak.setStatus(MammaAfspraakStatus.ONDERZOEK);
		onderzoek.setDoorgevoerd(false);
		screeningRonde.setLaatsteOnderzoek(onderzoek);
		if (verstuurHl7Berichten)
		{
			berichtToBatchService.queueMammaHL7v24BerichtUitgaand(dossier.getClient(), MammaHL7v24ORMBerichtStatus.STARTED);
		}
		hibernateService.saveOrUpdateAll(onderzoek, afspraak, screeningRonde, dossier);
		volgendeUitnodigingService.updateVolgendeUitnodigingNaDeelname(afspraak.getUitnodiging().getScreeningRonde().getDossier());
		return onderzoek;
	}

	private void setSignalerenVoorOnderzoek(InstellingGebruiker mbber, MammaOnderzoek onderzoek, boolean afwijking)
	{
		if (onderzoek.getSignaleren() == null)
		{
			var signaleren = new MammaSignaleren();
			signaleren.setHeeftAfwijkingen(afwijking);
			signaleren.setOnderzoek(onderzoek);
			signaleren.setAfgerondDoor(mbber);
			signaleren.setAfgerondOp(currentDateSupplier.getDate());
			onderzoek.setSignaleren(signaleren);
		}
	}

	private MammaOnderzoek maakOnderzoek(MammaAfspraak afspraak, MammaScreeningsEenheid se)
	{
		var onderzoek = new MammaOnderzoek();
		onderzoek.setCreatieDatum(currentDateSupplier.getDate());
		onderzoek.setAfspraak(afspraak);
		onderzoek.setScreeningsEenheid(se);
		onderzoek.setStatus(MammaOnderzoekStatus.ACTIEF);
		onderzoek.setOperatieRechts(Boolean.FALSE);
		onderzoek.setOperatieLinks(Boolean.FALSE);
		onderzoek.setAmputatie(null);
		onderzoek.setDoorgevoerd(false);
		onderzoek.setOnderzoekType(MammaOnderzoekType.MAMMOGRAFIE);

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

	private MammaMammografie maakMammaMammografie(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker)
	{
		return baseFactory.maakMammografie(onderzoek, instellingGebruiker, new MammaAnnotatieAfbeelding());
	}

	@Override
	@Transactional
	public void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker)
	{
		voegLezingToe(beoordeling, lezing, gebruiker, true);
	}

	@Override
	@Transactional
	public void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker, boolean verstuurHl7Berichten)
	{
		if (MammaBeoordelingStatus.VERSLAG_MAKEN != beoordeling.getStatus())
		{
			baseBeoordelingService.slaLezingOpEnVerwerkStatus(beoordeling, lezing, gebruiker, b -> "Niet opgeschort");
		}
		else
		{
			baseBeoordelingService.slaLezingOp(beoordeling, lezing);
			baseBeoordelingService.setStatusNaarVerslagGereed(beoordeling);
		}

		baseBeoordelingService.bevestigLezing(beoordeling, verstuurHl7Berichten);
	}

	@Override
	@Transactional
	public void voegEersteTweeLezingenToe(MammaBeoordeling beoordeling, MammaLezing lezing1, MammaLezing lezing2, InstellingGebruiker instellingGebruiker,
		boolean verstuurHl7Berichten)
	{
		voegLezingToe(beoordeling, lezing1, instellingGebruiker, verstuurHl7Berichten);
		voegLezingToe(beoordeling, lezing2, instellingGebruiker, verstuurHl7Berichten);
	}

	@Override
	@Transactional
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (var bsn : model.getBsns())
		{
			var client = maakOfVindClient(model, bsn);
			clienten.add(client);
		}
		return clienten;
	}

	@Override
	@Transactional
	public List<Client> maakOfWijzigClienten(TestTimelineModel model)
	{
		var clienten = new ArrayList<Client>();
		for (var bsn : model.getBsns())
		{
			var client = maakOfVindClient(model, bsn);
			clienten.add(client);
			var gbaPersoon = client.getPersoon();
			gbaPersoon.setGeslacht(model.getGeslacht());
			gbaPersoon.setGeboortedatum(model.getGeboortedatum());
			gbaPersoon.getGbaAdres().setPostcode(model.getPostcode());
			var dossier = client.getMammaDossier();
			dossier.setDoelgroep(model.getDoelgroep());

			var deelnamekans = dossier.getDeelnamekans();
			deelnamekans.setDeelnamekans(model.getDeelnamekans());

			for (var screeningRonde : dossier.getScreeningRondes())
			{
				for (var uitnodiging : screeningRonde.getUitnodigingen())
				{
					for (var afspraak : uitnodiging.getAfspraken())
					{
						var opkomstkans = afspraak.getOpkomstkans();
						opkomstkans.setOpkomstkans(model.getDeelnamekans());
						hibernateService.saveOrUpdate(opkomstkans);
					}
				}
			}

			transgenderService.bijwerkenDeelnamemodus(client);
			hibernateService.saveOrUpdateAll(gbaPersoon, dossier, deelnamekans);
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		var persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeslacht(model.getGeslacht());
		persoon.setGeboortedatum(model.getGeboortedatum());

		var adres = new BagAdres();
		adres.setGbaGemeente(model.getGemeente());
		adres.setPostcode(model.getPostcode());
		persoon.setGbaAdres(adres);
		var deelnamekans = model.getDeelnamekans();
		return baseTestService.maakOfVindClient(persoon, model.getDoelgroep(), deelnamekans, deelnamekans, true);
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{
		Client eersteClient = null;
		var aantalUitnodigingen = 0;
		List<String> errorMeldingen = new ArrayList<>();
		for (var client : clienten)
		{
			if (eersteClient == null)
			{
				eersteClient = client;
				aantalUitnodigingen = getAantalUitnodigingen(client);
			}
			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				var error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
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
		var aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && !screeningRonde.getUitnodigingen().isEmpty())
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
	@Transactional
	public int importPocClienten(File file, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid screeningsEenheid, ImportPocOpties importPocOpties)
	{
		var klaarzettenPlanning = importPocOpties.equals(ImportPocOpties.KLAARZETTEN_VOOR_PLANNING);
		var aantal = 0;
		try (var reader = new CSVReader(new FileReader(file), ';'))
		{
			reader.readNext(); 
			String[] vorigeLine = null;
			var huidigeLine = reader.readNext();
			var formatGeboortedatum = new SimpleDateFormat("yyyy-MM-dd");
			while (huidigeLine != null)
			{
				var bsn = huidigeLine[0];
				var naam = huidigeLine[1];
				var gebdatum = huidigeLine[3];
				var uitnodigingsId = huidigeLine[4];
				var postcode = huidigeLine[11];
				var huisnummer = huidigeLine[12];
				var beoordeling = huidigeLine[13];
				var filterPersoon = new GbaPersoon();
				filterPersoon.setBsn(StringUtils.leftPad(bsn, 9, '0'));
				filterPersoon.setGeslacht(Geslacht.VROUW);

				var gbaAdres = new BagAdres();
				gbaAdres.setPostcode(StringUtils.isNotBlank(postcode) ? postcode : "1234AA");
				gbaAdres.setHuisnummer(StringUtils.isNotBlank(huisnummer) ? Integer.parseInt(huisnummer) : 9);
				gbaAdres.setGbaGemeente(hibernateService.loadAll(Gemeente.class).get(0));
				filterPersoon.setGbaAdres(gbaAdres);
				var dossier = baseTestService.geefDossier(filterPersoon);
				var deelnamekans = dossier.getDeelnamekans();
				deelnamekans.setDeelnamekans(BigDecimal.valueOf(0.8));
				hibernateService.saveOrUpdateAll(dossier, deelnamekans);

				var persoon = dossier.getClient().getPersoon();
				var naamSplitted = naam.split("\\^");
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

					var onderzoek = voegPocOnderzoekToe(dossier, screeningsEenheid, instellingGebruiker);
					voegPocBeoordelingToe(onderzoek, instellingGebruiker, vorigeLine[13]);
				}

				MammaStandplaatsRonde standplaatsRonde = null;
				if (klaarzettenPlanning)
				{
					var standplaats = standplaatsService.getStandplaatsMetPostcode(dossier.getClient());
					standplaatsRonde = hibernateService.loadAll(MammaStandplaatsRonde.class).stream().filter(sr -> !sr.getStandplaats().equals(standplaats))
						.findFirst().orElse(hibernateService.loadAll(MammaStandplaatsRonde.class).get(0));
				}

				var rekenDossierTerug = !klaarzettenPlanning;

				var uitnodiging = baseTestTimelineService.nieuweRondeAfspraakUitnodiging(dossier.getClient(), screeningsEenheid, standplaatsRonde,
					rekenDossierTerug);
				uitnodiging.getScreeningRonde().setUitnodigingsNr(Long.valueOf(uitnodigingsId));
				hibernateService.saveOrUpdate(uitnodiging);

				if (!importPocOpties.equals(ImportPocOpties.KLAARZETTEN_VOOR_SE))
				{
					var onderzoek = voegPocOnderzoekToe(dossier, screeningsEenheid, instellingGebruiker);

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
		var onderzoek = MammaScreeningRondeUtil.getLaatsteOnderzoek(dossier.getLaatsteScreeningRonde());
		if (onderzoek == null)
		{
			onderzoek = maakOnderzoekVoorBe(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak(), instellingGebruiker, screeningsEenheid);
		}
		return onderzoek;
	}

	private void voegPocBeoordelingToe(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker, String beoordelingStatus)
	{
		var mammaBeoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(onderzoek);

		if (mammaBeoordeling == null)
		{
			throw new IllegalStateException("Er was geen beoordeling");
		}

		if (mammaBeoordeling.getEersteLezing() == null || mammaBeoordeling.getTweedeLezing() == null)
		{
			var radiologen = getTweeVerschillendeActieveErvanenRadioloog(mammaBeoordeling.getBeoordelingsEenheid());
			voegPocLezingToe(mammaBeoordeling, beoordelingStatus, radiologen[0]);
			var tweedeLezing = voegPocLezingToe(mammaBeoordeling, beoordelingStatus, radiologen[1]);

			if (beoordelingStatus.equals("ONGUNSTIG"))
			{
				var verslaglezing = baseBeoordelingService.maakVerslagLezing(mammaBeoordeling, tweedeLezing, instellingGebruiker, false);
				verslaglezing.setBiradsLinks(MammaBIRADSWaarde.VIJF);
				verslaglezing.setBiradsRechts(MammaBIRADSWaarde.VIER);
				voegLezingToe(mammaBeoordeling, verslaglezing, instellingGebruiker);
				verslagGoedkeurenDoorCE(mammaBeoordeling, instellingGebruiker);
			}
			var screeningRonde = mammaBeoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
			screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			screeningRonde.setStatusDatum(onderzoek.getAfgerondOp());

		}
	}

	private MammaLezing voegPocLezingToe(MammaBeoordeling mammaBeoordeling, String beoordelingStatus, InstellingGebruiker beoordelaar)
	{
		var lezing = beoordelingService.getOrCreate1eOf2eLezing(mammaBeoordeling, beoordelaar, true);

		if (beoordelingStatus.equals("ONGUNSTIG"))
		{
			lezing.setBiradsLinks(MammaBIRADSWaarde.VIJF);
			lezing.setBiradsRechts(MammaBIRADSWaarde.VIER);
		}
		voegLezingToe(mammaBeoordeling, lezing, beoordelaar);
		return lezing;
	}

	@Override
	@Transactional
	public void verslagGoedkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker)
	{
		beoordelingService.verslagGoedkeurenDoorCE(beoordeling, false, null, ingelogdeGebruiker);
	}

	@Override
	@Transactional
	public void doorvoerenOnderzoek(MammaAfspraak afspraak)
	{
		onderzoekService.onderzoekDoorvoerenVanuitSe(afspraak.getOnderzoek());
	}

	@Override
	@Transactional
	public void beeldenBeschikbaarBe(MammaAfspraak afspraak)
	{
		try
		{
			var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
			onderzoekService.ontvangBeeldenVoorOnderzoek(screeningRonde.getDossier().getClient(), screeningRonde, afspraak.getOnderzoek().getOnderzoekType());
		}
		catch (HL7Exception e)
		{
			LOG.error("Fout bij beelden als ontvangen markeren", e);
		}
	}

	private InstellingGebruiker[] getTweeVerschillendeActieveErvanenRadioloog(BeoordelingsEenheid beoordelingsEenheid)
	{
		var zoekInstellingGebruiker = new InstellingGebruiker();
		zoekInstellingGebruiker.setOrganisatie(beoordelingsEenheid);
		var instellingGebruikers = medewerkerService.getActieveRadiologen(zoekInstellingGebruiker, new ArrayList<>(), "medewerker.gebruikersnaam", true)
			.stream()
			.filter(m -> beoordelingService.isBevoegdVoorArbitrage(m))
			.collect(Collectors.toList());
		var iterator = instellingGebruikers.iterator();

		var radiologen = new InstellingGebruiker[2];
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
	@Transactional
	public void doorvoerenAdhocMeekijkverzoek(MammaOnderzoek onderzoek)
	{
		baseFactory.maakAdhocMeekijkverzoek(onderzoek, "Op verzoek van SE");
	}

	@Override
	@Transactional
	public void doorvoerenOnderzoekStarten(MammaAfspraak afspraak, InstellingGebruiker ingelogdeInstellingGebruiker, boolean verstuurHl7Berichten)
	{
		onderzoekStarten(afspraak, ingelogdeInstellingGebruiker, verstuurHl7Berichten);
	}

	@Override
	public boolean isSnelkeuzeKnopMammaBeschikbaar(Client client, TestTimelineRonde timeLineRonde)
	{
		var persoon = client.getPersoon();
		var isOverleden = persoon.getOverlijdensdatum() != null;
		var gebeurtenissen = timeLineRonde.getMammaScreeningRondeDossier();
		var ronde = (MammaScreeningRonde) gebeurtenissen.getScreeningRonde();
		MammaOnderzoek onderzoek = null;
		if (ronde.getLaatsteUitnodiging() != null && ronde.getLaatsteUitnodiging().getLaatsteAfspraak() != null)
		{
			onderzoek = ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
		}
		var onderbrokenZonderFotosSitautie = onderzoek != null && magBeeldenBeschikbaarMaken(onderzoek)
			&& MammaOnderzoekStatus.ONVOLLEDIG.equals(onderzoek.getStatus()) && OnvolledigOnderzoekOption.ZONDER_FOTOS.equals(onderzoek.getOnvolledigOnderzoek())
			&& ronde.getDossier().getLaatsteScreeningRonde().equals(ronde);
		var isLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus());
		boolean isAangemeld = ronde.getAangemeld();
		var heeftGunstigeUitslag =
			onderzoek != null && onderzoek.getLaatsteBeoordeling() != null && MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(onderzoek.getLaatsteBeoordeling().getStatus());
		var heeftFollowUpConclusie = ronde.getFollowUpConclusieStatus() != null;
		return !isOverleden && (isLopend || !isAangemeld || onderbrokenZonderFotosSitautie || (heeftGunstigeUitslag && !heeftFollowUpConclusie));
	}

	@Override
	@Transactional
	public void registreerDeelnamewens(Client client)
	{
		var dto = deelnamemodusService.getDeelnamewensDto(client);
		dto.setDeelnamewensBk(true);
		deelnamemodusDossierService.pasDeelnamewensToe(client, dto, null);
	}

	@Override
	@Transactional
	public void sluitAlleDagenTotEnMetGisteren(MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeGebruiker) throws IllegalStateException
	{
		var afspraken = readAfsprakenWaarvanOnderzoekNietIsDoorgevoerd(currentDateSupplier.getLocalDate(), screeningsEenheid.getCode());

		var nietAfgerondeAfspraken = new ArrayList<MammaAfspraak>();
		for (var afspraak : afspraken)
		{
			try
			{
				setMammaAfspraakStatusGeplandOfVoerOnderzoekDoor(afspraak, ingelogdeGebruiker);
			}
			catch (Exception e)
			{
				LOG.error("Afsluiten van dagen ging niet goed door client met id: {}", afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId(), e);
				nietAfgerondeAfspraken.add(afspraak);
			}
		}
		if (!nietAfgerondeAfspraken.isEmpty())
		{
			var bsns = nietAfgerondeAfspraken.stream().map(a -> a.getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon().getBsn())
				.collect(Collectors.joining(","));
			throw new IllegalStateException("Afsluiten van dagen ging niet goed door de volgende BSN's: " + bsns);
		}
	}

	private void setMammaAfspraakStatusGeplandOfVoerOnderzoekDoor(MammaAfspraak afspraak, InstellingGebruiker ingelogdeGebruiker)
	{
		var onderzoek = afspraak.getOnderzoek();

		if (onderzoek == null)
		{
			afspraak.setStatus(MammaAfspraakStatus.GEPLAND);
			hibernateService.saveOrUpdate(afspraak);
		}
		else
		{
			if (onderzoek.getStatus() == MammaOnderzoekStatus.ACTIEF || afspraak.getStatus() != MammaAfspraakStatus.BEEINDIGD)
			{
				var afwijkingGesignaleerd = isAfwijkingGesignaleerd(onderzoek);
				var onderzoeker = bepaalOnderzoeker(afspraak, ingelogdeGebruiker);
				rondOnderzoekAf(afspraak, onderzoeker, false, onderzoek.getOnvolledigOnderzoek(), onderzoek.getOnderbrokenOnderzoek(), onderzoek.getOnderzoekType(),
					afwijkingGesignaleerd, null);
			}
			doorvoerenOnderzoek(afspraak);
		}

	}

	private static boolean isAfwijkingGesignaleerd(MammaOnderzoek onderzoek)
	{
		return onderzoek.getSignaleren() != null && onderzoek.getSignaleren().isHeeftAfwijkingen();
	}

	private static InstellingGebruiker bepaalOnderzoeker(MammaAfspraak afspraak, InstellingGebruiker ingelogdeGebruiker)
	{
		var mammografie = afspraak.getOnderzoek().getMammografie();
		if (mammografie != null && mammografie.getAfgerondDoor() != null)
		{
			return mammografie.getAfgerondDoor();
		}
		else if (afspraak.getIngeschrevenDoor() != null)
		{
			return afspraak.getIngeschrevenDoor();
		}
		return ingelogdeGebruiker;
	}

	@Override
	public String getBsnsMetBeeldenBeschikbaar()
	{
		return clientRepository.findAll(MammaMammografieSpecification.heeftBeeldenBeschikbaar())
			.stream().map(client -> client.getPersoon().getBsn()).collect(Collectors.joining(","));
	}

	@Override
	public String clientenResetten(String bsns)
	{
		var gevondenEnIsVerwijderd = 0;
		var bsnList = bsns.split(",");
		var result = "Succesvol";
		for (var bsn : bsnList)
		{
			try
			{
				if (org.apache.commons.lang.StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				var client = clientRepository.findOne(ClientRepository.bsnEquals(bsn)).orElse(null);
				if (client == null)
				{
					continue;
				}
				baseTestService.clientReset(client, false);
				testService.verwijderClientContacten(client, Bevolkingsonderzoek.MAMMA);
				gevondenEnIsVerwijderd++;
			}
			catch (Exception e)
			{
				result = "Fout bij resetten van client met BSN " + bsn;
				LOG.error("error bij bsn " + bsn, e);
			}
		}
		return result + ". #" + gevondenEnIsVerwijderd + " clienten gereset.";
	}

	@Override
	public List<MammaAfspraak> readAfsprakenWaarvanOnderzoekNietIsDoorgevoerd(LocalDate vandaag, String seCode)
	{
		return baseAfspraakRepository.findAll(afsprakenWaarvanOnderzoekNietIsDoorgevoerdAfgelopen2Maanden(vandaag, seCode));
	}
}
