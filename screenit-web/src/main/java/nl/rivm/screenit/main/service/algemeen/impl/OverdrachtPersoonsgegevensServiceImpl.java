package nl.rivm.screenit.main.service.algemeen.impl;

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

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.cervix.CervixRondeDao;
import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.util.CervixCisHistoryUtil;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorieOngestructureerdRegel;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.beanutils.PropertyUtilsBean;
import org.apache.commons.beanutils.expression.Resolver;
import org.apache.commons.lang.reflect.FieldUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.cfg.MapperConfig;
import com.fasterxml.jackson.databind.introspect.AnnotatedMethod;
import com.fasterxml.jackson.databind.module.SimpleModule;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class OverdrachtPersoonsgegevensServiceImpl implements OverdrachtPersoonsgegevensService
{
	private static final Logger LOG = LoggerFactory.getLogger(OverdrachtPersoonsgegevensServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private RondeNummerService rondeNummerService;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixRondeDao cervixRondeDao;

	@Autowired
	private MammaBaseLaesieService baseLaesieService;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Override
	public boolean heeftVerzoekZonderGegenereerdeBrief(Client client)
	{
		return briefService.clientHeeftOngegenereerdeBriefVanType(BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_AANVRAAG, client, AlgemeneBrief.class);
	}

	@Override
	public void slaOntvangenFormulierOp(OverdrachtPersoonsgegevens overdracht, UploadDocument uploadDocument, Account account) throws IOException
	{
		if (overdracht.getOntvangenAanvraagbrief() == null)
		{
			saveOrUpdateGetekendFormulier(overdracht, uploadDocument);
			overdracht.setOntvangenAanvraagbrief(uploadDocument);
			overdracht.setStatus(AanvraagBriefStatus.BRIEF_ONTVANGEN);
			overdracht.setStatusDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(overdracht);
			logService.logGebeurtenis(LogGebeurtenis.CLIENT_OVERDRACHT_PERSOONSGEGEVENS_GEUPLOAD, account, overdracht.getClient());
		}
		else
		{
			vervangGetekendeFormulier(overdracht, uploadDocument, account);
		}
	}

	private void vervangGetekendeFormulier(OverdrachtPersoonsgegevens overdracht, UploadDocument nieuwDocument, Account account) throws IOException
	{
		fileService.delete(overdracht.getOntvangenAanvraagbrief(), true);
		overdracht.setOntvangenAanvraagbrief(nieuwDocument);
		saveOrUpdateGetekendFormulier(overdracht, nieuwDocument);
		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, account, overdracht.getClient(), "Formulier inzage/overdracht persoonsgegevens is vervangen.");
	}

	@Override
	public void verstuurGeenHandtekeningBrief(OverdrachtPersoonsgegevens overdracht, Account loggedInAccount)
	{
		AlgemeneBrief brief = briefService.maakAlgemeneBrief(overdracht.getClient(), BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_HANDTEKENING);
		overdracht.setGeenHandtekeningBrief(brief);
		overdracht.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(overdracht);
	}

	private void saveOrUpdateGetekendFormulier(OverdrachtPersoonsgegevens overdracht, UploadDocument uploadDocument) throws IOException
	{
		fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.OVERDRACHT_PERSOONSGEVENS, overdracht.getClient().getId());
	}

	@Override
	public void afronden(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens)
	{
		overdrachtPersoonsgegevens.setStatus(AanvraagBriefStatus.VERWERKT);
		overdrachtPersoonsgegevens.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(overdrachtPersoonsgegevens);
	}

	@Override
	public void createDataDump(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens, OutputStream outputStream, Account account)
	{
		try
		{

			Workbook workbook = new XSSFWorkbook();

			CreationHelper createHelper = workbook.getCreationHelper();

			CellStyle cellStyleDate = workbook.createCellStyle();
			CellStyle cellStyleDateTime = workbook.createCellStyle();

			cellStyleDate.setDataFormat(createHelper.createDataFormat().getFormat(Constants.DEFAULT_DATE_FORMAT));
			cellStyleDateTime.setDataFormat(createHelper.createDataFormat().getFormat("dd-MM-yyyy HH:mm:ss"));

			Client client = overdrachtPersoonsgegevens.getClient();
			GbaPersoon persoon = client.getPersoon();
			addClientGegevens(workbook, createHelper, cellStyleDate, persoon);
			if (overdrachtPersoonsgegevens.getDkGegevens())
			{
				addDkGegevens(workbook, createHelper, cellStyleDate, cellStyleDateTime, client.getColonDossier());
			}
			if (overdrachtPersoonsgegevens.getBmhkGegevens())
			{
				addBmhkGegevens(workbook, createHelper, cellStyleDate, cellStyleDateTime, client.getCervixDossier());
			}
			if (overdrachtPersoonsgegevens.getBkGegevens())
			{
				addBkGegevens(workbook, createHelper, cellStyleDate, cellStyleDateTime, client.getMammaDossier());
			}
			workbook.write(outputStream);
			logService.logGebeurtenis(LogGebeurtenis.CLIENT_OVERDRACHT_PERSOONSGEGEVENS_GEDOWNLOAD, account, overdrachtPersoonsgegevens.getClient());
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
	}

	private void addBkGegevens(Workbook workbook, CreationHelper createHelper, CellStyle cellStyleDate, CellStyle cellStyleDateTime, MammaDossier mammaDossier)
	{
		Sheet sheet = workbook.createSheet(Bevolkingsonderzoek.MAMMA.getNaam());
		List<MammaScreeningRonde> rondes = new ArrayList<>(mammaDossier.getScreeningRondes());
		Collections.sort(rondes, new PropertyComparator<>("creatieDatum", false, true));

		addRow(sheet, "Doelgroep", StringUtil.enumName2readableString(mammaDossier.getDoelgroep().name()), workbook, null);
		for (MammaScreeningRonde ronde : rondes)
		{
			addRow(sheet, "Ronde", rondeNummerService.geefRondeNummer(ronde), ronde.getCreatieDatum(), workbook, cellStyleDateTime);
			addBkHuisarts(workbook, cellStyleDateTime, sheet, ronde);
			addBkOnderzoeken(workbook, cellStyleDateTime, sheet, ronde);
			addBKFollowUpVerslagen(workbook, cellStyleDateTime, sheet, ronde);
			addEmptyRow(sheet);
		}
	}

	private void addBkHuisarts(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaScreeningRonde ronde)
	{
		EnovationHuisarts huisarts = ronde.getHuisarts();
		if (huisarts != null)
		{
			addRow(sheet, "Huisarts", getVolledigeHuisArtsTekst(huisarts), ronde.getDatumVastleggenHuisarts(), workbook, cellStyleDateTime);
		}
		else
		{
			MammaGeenHuisartsOption geenHuisartsOptie = ronde.getGeenHuisartsOptie();
			if (geenHuisartsOptie != null)
			{
				addRow(sheet, "Huisarts", geenHuisartsOptie.name(), ronde.getDatumVastleggenHuisarts(), workbook, cellStyleDateTime);
			}
		}
	}

	private void addBkOnderzoeken(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaScreeningRonde ronde)
	{
		List<MammaUitnodiging> uitnodigingen = ronde.getUitnodigingen();
		for (MammaUitnodiging uitnodiging : uitnodigingen)
		{
			for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
			{
				addBkAfspraakIdentificatie(workbook, cellStyleDateTime, sheet, afspraak);
				addBkOnderzoekSe(workbook, cellStyleDateTime, sheet, afspraak);
				addBkBeoordelingen(workbook, cellStyleDateTime, sheet, afspraak.getOnderzoek());
			}
		}
	}

	private void addBkLaesies(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaBeoordeling beoordeling)
	{
		if (beoordeling.getVerslagLezing() != null)
		{
			String laesieString = baseLaesieService.getAllLaesieTekstVoorVerslagLezing(beoordeling.getVerslagLezing());
			addRow(sheet, "Laesies", laesieString, beoordeling.getStatusDatum(), workbook, cellStyleDateTime);
		}
	}

	private void addBkNevenbevindingen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaBeoordeling beoordeling)
	{
		if (baseBeoordelingService.heeftBeoordelingNevenbevindingen(beoordeling))
		{
			String nevenbevindingenString = baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, beoordeling.getEersteLezing(),
				beoordeling.getTweedeLezing());
			String nevenbevindingOpmerkingTekst = baseBeoordelingService.getNevenbevindingOpmerkingTekst("\n", beoordeling.getEersteLezing(), beoordeling.getTweedeLezing());
			addRow(sheet, "Nevenbevindingen", nevenbevindingenString, beoordeling.getStatusDatum(), workbook, cellStyleDateTime);
			if (nevenbevindingOpmerkingTekst != null)
			{
				addRow(sheet, "Nevenbevindingen opmerking", nevenbevindingOpmerkingTekst, beoordeling.getStatusDatum(), workbook, cellStyleDateTime);
			}
		}
	}

	private void addBkBeoordelingen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaOnderzoek onderzoek)
	{
		if (onderzoek != null)
		{
			for (MammaBeoordeling beoordeling : onderzoek.getBeoordelingen())
			{
				if (MammaBeoordelingStatus.isUitslagStatus(beoordeling.getStatus()))
				{
					addRow(sheet, "Uitslag", beoordeling.getStatus().getNaam(), beoordeling.getStatusDatum(), workbook, cellStyleDateTime);
					addBkLaesies(workbook, cellStyleDateTime, sheet, beoordeling);
					addBkNevenbevindingen(workbook, cellStyleDateTime, sheet, beoordeling);
				}
			}
		}
	}

	private void addBkOnderzoekSe(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaAfspraak afspraak)
	{
		String label = "Afspraak";
		String status = StringUtil.enumName2readableString(afspraak.getStatus().name());
		String se = afspraak.getStandplaatsPeriode().getScreeningsEenheid().getNaam();
		Date datum = afspraak.getVanaf();

		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		if (onderzoek != null)
		{
			status = StringUtil.enumName2readableString(onderzoek.getStatus().name());
			se = onderzoek.getScreeningsEenheid().getNaam();
			datum = onderzoek.getAfgerondOp() != null ? onderzoek.getAfgerondOp() : onderzoek.getCreatieDatum();
			label = "Onderzoek";
		}
		addRow(sheet, label, status + " : " + se, datum, workbook, cellStyleDateTime);
	}

	private void addBkAfspraakIdentificatie(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaAfspraak afspraak)
	{
		String identificatienummer = afspraak.getIdentificatienummer();
		MammaIdentificatiesoort identificatieSoort = afspraak.getIdentificatiesoort();
		if (identificatieSoort != null)
		{
			addRow(sheet, "Identificatie",
				StringUtil.enumName2readableString(identificatieSoort.name()) + " : " + identificatienummer,
				afspraak.getIngeschrevenOp(),
				workbook, cellStyleDateTime);
		}
	}

	private void addBKFollowUpVerslagen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaScreeningRonde ronde)
	{
		for (MammaVerslag verslag : ronde.getFollowUpVerslagen())
		{
			addVerslagRow(workbook, cellStyleDateTime, sheet, verslag);
		}
		for (MammaFollowUpRadiologieVerslag radiologieVerslag : ronde.getFollowUpRadiologieVerslagen())
		{
			addMammaFollowUpRadiologieverslagRow(workbook, cellStyleDateTime, sheet, radiologieVerslag);
		}
	}

	private void addDkGegevens(Workbook workbook, CreationHelper createHelper, CellStyle cellStyleDate, CellStyle cellStyleDateTime, ColonDossier dossier)
	{
		Sheet sheet = workbook.createSheet(Bevolkingsonderzoek.COLON.getNaam());
		List<ColonScreeningRonde> rondes = new ArrayList<>(dossier.getScreeningRondes());
		Collections.sort(rondes, new PropertyComparator<ColonScreeningRonde>("creatieDatum", false, true));
		for (ColonScreeningRonde ronde : rondes)
		{
			addRow(sheet, "Ronde", rondeNummerService.geefRondeNummer(ronde), ronde.getCreatieDatum(), workbook, cellStyleDateTime);
			addDkHuisarts(workbook, cellStyleDateTime, sheet, ronde);
			addDkOnbekendeHuisarts(workbook, cellStyleDateTime, sheet, ronde);
			addFITUitslagen(workbook, cellStyleDateTime, sheet, ronde);
			addIntakeConclusies(workbook, cellStyleDate, cellStyleDateTime, sheet, ronde);
			addDkVerslagen(workbook, cellStyleDateTime, sheet, ronde);
			if (ronde.getDefinitiefVervolgbeleid() != null)
			{
				addRow(sheet, "Definitief vervolgbeleid", ronde.getDefinitiefVervolgbeleid(), ronde.getStatusDatum(), workbook, cellStyleDateTime);
			}
			addEmptyRow(sheet);
		}
		sheet.autoSizeColumn(0);
		sheet.autoSizeColumn(2);
	}

	private void addDkHuisarts(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, ColonScreeningRonde ronde)
	{
		EnovationHuisarts huisarts = ronde.getColonHuisarts();
		if (huisarts != null)
		{
			addRow(sheet, "Huisarts", getVolledigeHuisArtsTekst(huisarts), ronde.getDatumVastleggenHuisarts(), workbook, cellStyleDateTime);
		}
	}

	private void addDkOnbekendeHuisarts(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, ColonScreeningRonde ronde)
	{
		OnbekendeHuisarts onbekendeHuisarts = ronde.getOnbekendeHuisarts();
		if (onbekendeHuisarts != null)
		{
			String volledigeHuisartsTekst = "Huisarts: " + NaamUtil.getNaamOnbekendeHuisarts(onbekendeHuisarts) + ", Praktijk: " + onbekendeHuisarts.getPraktijkNaam()
				+ ", Adres: "
				+ AdresUtil.getOnbekendeHuisartsAdres(onbekendeHuisarts);
			addRow(sheet, "Onbekende huisarts", volledigeHuisartsTekst, ronde.getDatumVastleggenHuisarts(), workbook, cellStyleDateTime);
		}
	}

	private void addFITUitslagen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, ColonScreeningRonde ronde)
	{
		for (IFOBTTest test : ronde.getIfobtTesten())
		{
			if (test.getStatus() == IFOBTTestStatus.UITGEVOERD && test.getUitslag() != null)
			{
				String interpretatie = "";
				if (IFOBTTestUtil.isGunstig(test))
				{
					interpretatie = "(gunstig)";
				}
				else if (IFOBTTestUtil.isOngunstig(test))
				{
					interpretatie = "(ongunstig)";
				}
				addRow(sheet, "Uitslag FIT " + interpretatie, test.getUitslag().doubleValue(), test.getStatusDatum(), workbook, cellStyleDateTime);
			}
		}
	}

	private void addIntakeConclusies(Workbook workbook, CellStyle cellStyleDate, CellStyle cellStyleDateTime, Sheet sheet, ColonScreeningRonde ronde)
	{
		for (ColonIntakeAfspraak afspraak : ronde.getAfspraken())
		{
			ColonConclusie conclusie = afspraak.getConclusie();
			if (afspraak.getStatus() == AfspraakStatus.UITGEVOERD && conclusie != null)
			{
				addRow(sheet, "Conclusie intake afspraak", conclusie.getType(), conclusie.getDatum(), workbook, cellStyleDateTime);
				if (conclusie.getType() == ColonConclusieType.COLOSCOPIE)
				{
					addRow(sheet, "Datum coloscopie", conclusie.getDatumColoscopie(), conclusie.getDatum(), workbook, cellStyleDate, cellStyleDateTime);
				}
			}
		}
	}

	private void addDkVerslagen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, ColonScreeningRonde ronde)
	{
		for (ColonVerslag verslag : ronde.getVerslagen())
		{
			addVerslagRow(workbook, cellStyleDateTime, sheet, verslag);
		}
	}

	private void addBmhkGegevens(Workbook workbook, CreationHelper createHelper, CellStyle cellStyleDate, CellStyle cellStyleDateTime, CervixDossier dossier)
	{
		Sheet sheet = workbook.createSheet(Bevolkingsonderzoek.CERVIX.getNaam());
		List<CervixScreeningRonde> rondes = new ArrayList<>(dossier.getScreeningRondes());
		Collections.sort(rondes, new PropertyComparator<CervixScreeningRonde>("creatieDatum", false, true));
		for (CervixScreeningRonde ronde : rondes)
		{
			addRow(sheet, "Ronde", rondeNummerService.geefRondeNummer(ronde), ronde.getCreatieDatum(), workbook, cellStyleDateTime);
			addBmhkUitslagen(workbook, cellStyleDateTime, sheet, ronde);
			addBmhkVerslagen(workbook, cellStyleDateTime, sheet, ronde);
			addEmptyRow(sheet);
		}
		addCISHistorie(workbook, cellStyleDateTime, dossier, sheet);
		sheet.autoSizeColumn(0);
		sheet.autoSizeColumn(2);
	}

	private void addBmhkUitslagen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, CervixScreeningRonde ronde)
	{
		List<CervixMonster> ontvangenMonsters = cervixRondeDao.getOntvangenMonsters(ronde);
		for (CervixMonster monster : ontvangenMonsters)
		{
			addUitstrijkje(workbook, cellStyleDateTime, sheet, monster);
			addZAS(workbook, cellStyleDateTime, sheet, monster);
		}
	}

	private void addUitstrijkje(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, CervixMonster monster)
	{
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{
			CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
			addBmhkHuisarts(workbook, cellStyleDateTime, sheet, uitstrijkje);
			CervixHpvBeoordeling beoordeling = uitstrijkje.getLaatsteHpvBeoordeling();
			if (beoordeling != null && beoordeling.getHpvUitslag() != null)
			{
				addRow(sheet, "HPV uitslag (uitstrijkje)", beoordeling.getHpvUitslag().getNaam(), beoordeling.getAnalyseDatum(), workbook, cellStyleDateTime);
			}
		}
	}

	private void addBmhkHuisarts(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, CervixUitstrijkje uitstrijkje)
	{
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		if (labformulier != null && labformulier.getHuisartsLocatie() != null)
		{
			CervixHuisartsLocatie huisartsLocatie = labformulier.getHuisartsLocatie();
			CervixHuisarts huisarts = huisartsLocatie.getHuisarts();
			String volledigeHuisartsTekst = "Huisarts: " + NaamUtil.getNaamHuisarts(huisarts) + ", Praktijk: " + huisarts.getNaam() + ", Adres: "
				+ AdresUtil.getVolledigeAdresString(huisartsLocatie.getLocatieAdres()) + ", Locatie: " + huisartsLocatie.getNaam();
			addRow(sheet, "Uitstrijkend arts", volledigeHuisartsTekst, labformulier.getStatusDatum(), workbook, cellStyleDateTime);
		}
	}

	private void addZAS(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, CervixMonster monster)
	{
		if (CervixMonsterUtil.isZAS(monster))
		{
			CervixZas zas = CervixMonsterUtil.getZAS(monster);
			CervixHpvBeoordeling beoordeling = zas.getLaatsteHpvBeoordeling();
			if (beoordeling != null && beoordeling.getHpvUitslag() != null)
			{
				addRow(sheet, "HPV uitslag (ZAS)", beoordeling.getHpvUitslag().getNaam(), beoordeling.getAnalyseDatum(), workbook, cellStyleDateTime);
			}
		}
	}

	private void addBmhkVerslagen(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, CervixScreeningRonde ronde)
	{
		for (CervixVerslag verslag : ronde.getVerslagen())
		{
			addVerslagRow(workbook, cellStyleDateTime, sheet, verslag);
		}
	}

	private void addVerslagRow(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, Verslag verslag)
	{
		OntvangenCdaBericht ontvangenBericht = verslag.getOntvangenBericht();
		VerslagContent verslagContent = verslag.getVerslagContent();
		if (verslagContent != null)
		{
			String label = null;
			switch (verslag.getType())
			{
			case CERVIX_CYTOLOGIE:
				label = "Ontvangen cytologie verslag";
				break;
			case MDL:
				label = "Ontvangen coloscopie verslag";
				break;
			case PA_LAB:
				label = "Ontvangen pathologie verslag";
				break;
			case MAMMA_PA_FOLLOW_UP:
				label = "Ontvangen follow-up pathologie verslag";
				break;
			default:
				throw new IllegalArgumentException();
			}

			try
			{
				Date ontvangen = verslag.getDatumVerwerkt();
				if (ontvangenBericht != null)
				{
					ontvangen = ontvangenBericht.getOntvangen();
				}
				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
				mapper.addMixInAnnotations(Object.class, MixInByPropName.class);
				mapper.setSerializationInclusion(Include.NON_EMPTY);
				mapper.setPropertyNamingStrategy(new ReplaceNamingStrategy());
				mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
				SimpleModule simpleModule = new SimpleModule("SimpleModule", Version.unknownVersion());
				simpleModule.addSerializer(Date.class, new DateJsonSerializer());
				simpleModule.addSerializer(Boolean.class, new BooleanJsonSerializer());
				simpleModule.addSerializer(Quantity.class, new QuantityJsonSerializer());
				simpleModule.addSerializer(NullFlavourQuantity.class, new NullFlavourQuantityJsonSerializer());
				mapper.registerModule(simpleModule);
				ObjectWriter writer = mapper.writerWithDefaultPrettyPrinter();

				addRow(sheet, label, writer.writeValueAsString(HibernateHelper.deproxy(verslagContent)), ontvangen, workbook,
					cellStyleDateTime);
			}
			catch (JsonProcessingException e)
			{
				LOG.error("fout bij maken json van verslag " + verslag.getId(), e);
			}
		}
	}

	private void addMammaFollowUpRadiologieverslagRow(Workbook workbook, CellStyle cellStyleDateTime, Sheet sheet, MammaFollowUpRadiologieVerslag radiologieVerslag)
	{
		if (radiologieVerslag.getIngevoerdOp() != null)
		{
			String label = "Ontvangen follow-up radiologie verslag";
			Date ingevoerdOp = radiologieVerslag.getIngevoerdOp();
			List<String> rowStrings = new ArrayList<>();
			if (radiologieVerslag.getConclusieBirads() != null)
			{
				rowStrings.add(StringUtil.kvp2String("Birads", StringUtil.enumName2readableString(radiologieVerslag.getConclusieBirads().name())));
			}
			if (radiologieVerslag.getConclusieEersteUitslagRadiologie() != null)
			{
				rowStrings.add(StringUtil.kvp2String("Conclusie eerste uitslag radiologie", radiologieVerslag.getConclusieEersteUitslagRadiologie()));
			}
			if (radiologieVerslag.getPathologieUitgevoerd() != null)
			{
				rowStrings.add(StringUtil.kvp2String("Pathologie aangevraagd", StringUtil.boolean2String(radiologieVerslag.getPathologieUitgevoerd())));
			}
			if (radiologieVerslag.getRadioloogTumorGrootteClassificatie() != null)
			{
				rowStrings.add(StringUtil.kvp2String("Tumorgrootte classificatie", radiologieVerslag.getRadioloogTumorGrootteClassificatie().name()));
			}
			if (radiologieVerslag.getRadioloogTumorGrootte() != null)
			{
				rowStrings.add(StringUtil.kvp2String("Tumorgrootte in cm", radiologieVerslag.getRadioloogTumorGrootte().toString()));
			}
			if (!rowStrings.isEmpty())
			{
				String row = String.join(", ", rowStrings);
				addRow(sheet, label, row, ingevoerdOp, workbook, cellStyleDateTime);
			}
		}
	}

	@JsonIgnoreProperties(value = { "id" })
	private class MixInByPropName
	{
	}

	private static class ReplaceNamingStrategy extends PropertyNamingStrategy
	{

		private static final long serialVersionUID = 1L;

		@Override
		public String nameForGetterMethod(MapperConfig<?> config, AnnotatedMethod method, String defaultName)
		{
			Class<?> declaringClass = method.getDeclaringClass();
			if (HibernateObject.class.isAssignableFrom(declaringClass))
			{
				Field field = FieldUtils.getDeclaredField(declaringClass, defaultName, true);
				if (field != null)
				{
					VraagElement annotation = field.getAnnotation(VraagElement.class);
					if (annotation != null && annotation.displayName() != null)
					{
						defaultName = annotation.displayName();
					}
				}
			}
			return defaultName;
		}
	}

	private static class DateJsonSerializer extends JsonSerializer<Date>
	{

		@Override
		public void serialize(Date value, JsonGenerator jgen, SerializerProvider provider) throws IOException, JsonGenerationException
		{
			jgen.writeString(Constants.getDateFormat().format(value));
		}
	}

	private static class BooleanJsonSerializer extends JsonSerializer<Boolean>
	{

		@Override
		public void serialize(Boolean value, JsonGenerator jgen, SerializerProvider provider) throws IOException, JsonGenerationException
		{
			jgen.writeString(Boolean.TRUE.equals(value) ? "Ja" : "Nee");
		}
	}

	private static class NullFlavourQuantityJsonSerializer extends JsonSerializer<NullFlavourQuantity>
	{

		@Override
		public void serialize(NullFlavourQuantity value, JsonGenerator gen, SerializerProvider serializers) throws IOException, JsonProcessingException
		{
			String quantity = value.getValue();
			if (value.getUnit() != null)
			{
				quantity += " " + value.getUnit();
			}
			else if (StringUtils.isBlank(value.getValue()) && Boolean.TRUE.equals(value.getNullFlavour()))
			{
				quantity = "niet van toepassing";
			}
			gen.writeString(quantity);
		}

	}

	private static class QuantityJsonSerializer extends JsonSerializer<Quantity>
	{

		@Override
		public void serialize(Quantity value, JsonGenerator gen, SerializerProvider serializers) throws IOException, JsonProcessingException
		{
			String quantity = value.getValue();
			if (value.getUnit() != null)
			{
				quantity += " " + value.getUnit();
			}
			gen.writeString(quantity);
		}

	}

	private void addCISHistorie(Workbook workbook, CellStyle cellStyleDateTime, CervixDossier dossier, Sheet sheet)
	{
		if (dossier.getCisHistorie() != null)
		{
			Map<String, List<CervixCISHistorieOngestructureerdRegel>> ongestructureerdeRegelsPerRonde = CervixCisHistoryUtil
				.getOngestructureerdeRegelsPerRonde(dossier.getCisHistorie(), true);
			List<String> rondeVolgorde = CervixCisHistoryUtil.getOrderdKeys(ongestructureerdeRegelsPerRonde, true);
			addRow(sheet, "CIS historie", null, workbook, null);
			for (String ronde : rondeVolgorde)
			{
				addRow(sheet, "Ronde", ronde, workbook, null);
				for (CervixCISHistorieOngestructureerdRegel regel : ongestructureerdeRegelsPerRonde.get(ronde))
				{
					addRow(sheet, "Regel", regel.getTekst(), regel.getDatum(), workbook, cellStyleDateTime);
				}
			}
		}
	}

	private void addClientGegevens(Workbook workbook, CreationHelper createHelper, CellStyle cellStyleDate, GbaPersoon persoon)
	{
		Sheet sheet = workbook.createSheet("Clientgegevens");
		addGbaPersoonGegevens(workbook, sheet, createHelper, cellStyleDate, persoon);

		addGbaAdres(workbook, sheet, persoon);
		addTijdelijkGbaAdres(workbook, sheet, persoon);
		addTijdelijkAdres(workbook, sheet, persoon, cellStyleDate);
		sheet.autoSizeColumn(0);
		sheet.autoSizeColumn(1);
	}

	private void addGbaPersoonGegevens(Workbook workbook, Sheet sheet, CreationHelper createHelper, CellStyle cellStyleDate, GbaPersoon persoon)
	{
		CellStyle cellStyleGeboortedatum = workbook.createCellStyle();
		addRow(sheet, "Bsn", persoon, "bsn", workbook, null);
		addRow(sheet, "Anummer", persoon, "anummer", workbook, null);
		addRow(sheet, "Geslachtsnaam", persoon, "achternaam", workbook, null);
		addRow(sheet, "Geslachtsnaam tussenvoegsel", persoon, "tussenvoegsel", workbook, null);
		addRow(sheet, "Voornamen", persoon, "voornaam", workbook, null);
		addRow(sheet, "Naamgebruik", persoon, "naamGebruik", workbook, null);
		addRow(sheet, "Adellijke titel", persoon, "titel", workbook, null);
		String geboortedatumPattern = "dd-MM-yyyy";
		if (persoon.getGeboortedatumPrecisie() != null)
		{
			geboortedatumPattern = persoon.getGeboortedatumPrecisie().getDatePattern();
		}
		cellStyleGeboortedatum.setDataFormat(createHelper.createDataFormat().getFormat(geboortedatumPattern));
		addRow(sheet, "Geboortedatum", persoon, "geboortedatum", workbook, cellStyleGeboortedatum);
		addRow(sheet, "Geslacht", persoon, "geslacht", workbook, null);
		addRow(sheet, "Datum vestiging Nederland", persoon, "datumVestigingNederland", workbook, cellStyleDate);
		addRow(sheet, "Datum vertrokken uit Nederland", persoon, "datumVertrokkenUitNederland", workbook, cellStyleDate);
		addRow(sheet, "Partner achternaam", persoon, "partnerAchternaam", workbook, null);
		addRow(sheet, "Partner tussenvoegsel", persoon, "partnerTussenvoegsel", workbook, cellStyleDate);
		addRow(sheet, "Datum aangaan partnerschap", persoon, "datumAangaanPartnerschap", workbook, cellStyleDate);
		addRow(sheet, "Datum ontbinding partnerschap", persoon, "datumOntbindingPartnerschap", workbook, cellStyleDate);
	}

	private void addGbaAdres(Workbook workbook, Sheet sheet, GbaPersoon persoon)
	{
		BagAdres gbaAdres = persoon.getGbaAdres();
		addAdres(workbook, sheet, gbaAdres, "BRP ");
		addRow(sheet, "BRP Locatie beschrijving", gbaAdres, "locatieBeschrijving", workbook, null);
		addRow(sheet, "BRP Gemeente", gbaAdres, "gbaGemeente.naam", workbook, null);
	}

	private void addTijdelijkGbaAdres(Workbook workbook, Sheet sheet, GbaPersoon persoon)
	{
		Adres adres = persoon.getTijdelijkGbaAdres();
		if (adres != null)
		{
			addAdres(workbook, sheet, adres, "Tijdelijk BRP ");
		}
	}

	private void addTijdelijkAdres(Workbook workbook, Sheet sheet, GbaPersoon persoon, CellStyle cellStyleDate)
	{
		TijdelijkAdres adres = persoon.getTijdelijkAdres();
		if (adres != null)
		{
			addAdres(workbook, sheet, adres, "Tijdelijk ");
			addRow(sheet, "Tijdelijk adres startdatum", adres, "startDatum", workbook, cellStyleDate);
			addRow(sheet, "Tijdelijk adres einddatum", adres, "eindDatum", workbook, cellStyleDate);
		}
	}

	private void addAdres(Workbook workbook, Sheet sheet, Adres adres, String labelPrefix)
	{
		addRow(sheet, labelPrefix + "Straat", adres, "straat", workbook, null);
		addRow(sheet, labelPrefix + "Huisnummer", adres, "huisnummer", workbook, null);
		addRow(sheet, labelPrefix + "Huisletter", adres, "huisletter", workbook, null);
		addRow(sheet, labelPrefix + "Huisnummer toevoeging", adres, "huisnummerToevoeging", workbook, null);
		addRow(sheet, labelPrefix + "Huisnummer aanduiding", adres, "huisnummerAanduiding", workbook, null);
		addRow(sheet, labelPrefix + "Postcode", adres, "postcode", workbook, null);
		addRow(sheet, labelPrefix + "Plaats", adres, "plaats", workbook, null);
		addRow(sheet, labelPrefix + "Locatie beschrijving", adres, "locatieBeschrijving", workbook, null);
	}

	private void addEmptyRow(Sheet sheet)
	{
		addRow(sheet, null, null, null, null);
	}

	private void addRow(Sheet sheet, String label, Object rootObject, String property, Workbook workbook, CellStyle style)
	{
		try
		{
			Object value = PropertyUtilsBean2.getInstance().getNestedProperty(rootObject, property);
			if (value != null)
			{
				addRow(sheet, label, value, workbook, style);
			}
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			LOG.error("Fout bij ophalen value", e);
		}
	}

	private void addRow(Sheet sheet, String label, Object value, Date datumTijd, Workbook workbook, CellStyle dateStyle, CellStyle dateTimeStyle)
	{
		Row row = addRow(sheet, label, value, workbook, dateStyle);
		if (datumTijd != null)
		{
			Cell cell = row.createCell(2);
			addCellValue(cell, datumTijd, dateTimeStyle);
		}
	}

	private void addRow(Sheet sheet, String label, Object value, Date datumTijd, Workbook workbook, CellStyle dateTimeStyle)
	{
		addRow(sheet, label, value, datumTijd, workbook, null, dateTimeStyle);
	}

	private Row addRow(Sheet sheet, String label, Object value, Workbook workbook, CellStyle style)
	{
		Row row = sheet.createRow(sheet.getPhysicalNumberOfRows());
		Cell cell = row.createCell(0);
		addCellValue(cell, label, style);

		cell = row.createCell(1);
		addCellValue(cell, value, style);
		return row;
	}

	private void addCellValue(Cell cell, Object cellValue, CellStyle style)
	{
		if (cellValue instanceof Double)
		{
			cell.setCellValue(((Double) cellValue).doubleValue());
		}
		else if (cellValue instanceof Date)
		{
			cell.setCellValue((Date) cellValue);
			cell.setCellStyle(style);
		}
		else if (cellValue != null)
		{
			cell.setCellValue(cellValue.toString());
		}
	}

	private String getVolledigeHuisArtsTekst(EnovationHuisarts huisarts)
	{
		return "Huisarts: " + NaamUtil.getNaamHuisarts(huisarts) + ", Praktijk: " + huisarts.getPraktijknaam() + ", Adres: "
			+ AdresUtil.getVolledigeAdresString(huisarts.getAdres());
	}

	static class PropertyUtilsBean2 extends PropertyUtilsBean
	{
		private static PropertyUtilsBean2 instance;

		protected static PropertyUtilsBean2 getInstance()
		{
			if (instance == null)
			{
				instance = new PropertyUtilsBean2();
			}
			return instance;
		}

		@Override
		public Object getNestedProperty(Object bean, String name) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException
		{

			if (bean == null)
			{
				throw new IllegalArgumentException("No bean specified");
			}
			if (name == null)
			{
				throw new IllegalArgumentException("No name specified for bean class '" +
					bean.getClass() + "'");
			}

			Resolver resolver = getResolver();
			while (resolver.hasNested(name))
			{
				String next = resolver.next(name);
				Object nestedBean = null;
				if (bean instanceof Map)
				{
					nestedBean = getPropertyOfMapBean((Map) bean, next);
				}
				else if (resolver.isMapped(next))
				{
					nestedBean = getMappedProperty(bean, next);
				}
				else if (resolver.isIndexed(next))
				{
					nestedBean = getIndexedProperty(bean, next);
				}
				else
				{
					nestedBean = getSimpleProperty(bean, next);
				}
				if (nestedBean == null)
				{
					return null;
				}
				bean = nestedBean;
				name = resolver.remove(name);
			}

			if (bean instanceof Map)
			{
				bean = getPropertyOfMapBean((Map) bean, name);
			}
			else if (resolver.isMapped(name))
			{
				bean = getMappedProperty(bean, name);
			}
			else if (resolver.isIndexed(name))
			{
				bean = getIndexedProperty(bean, name);
			}
			else
			{
				bean = getSimpleProperty(bean, name);
			}
			return bean;

		}
	}
}
