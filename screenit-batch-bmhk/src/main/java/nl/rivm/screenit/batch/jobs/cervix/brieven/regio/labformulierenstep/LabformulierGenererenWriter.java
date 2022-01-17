package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.JavaScriptPdfHelper;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.validator.routines.IBANValidator;
import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.interactive.action.PDActionJavaScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

public class LabformulierGenererenWriter implements ItemStreamWriter<Long>
{

	private static final Logger LOG = LoggerFactory.getLogger(LabformulierGenererenWriter.class);

	private static int MAXBRIEVENPDF = 500;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private FileService fileService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	private int volgnummerBatch;

	private int volgnummerDocument;

	private CervixRegioMergedBrieven voorbladerenMergedBrieven;

	private CervixRegioMergedBrieven labFormulierenMergedBrieven;

	private JobExecution jobExecution;

	private StepExecution stepExecution;

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		voorbladerenMergedBrieven = null;
		volgnummerBatch = 0;
		labFormulierenMergedBrieven = null;
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	@Override
	public void close() throws ItemStreamException
	{
		if (!ExitStatus.FAILED.equals(stepExecution.getExitStatus()))
		{
			if (labFormulierenMergedBrieven != null)
			{
				voegPrintFunctionaliteitToe(labFormulierenMergedBrieven);
				hibernateService.saveOrUpdate(labFormulierenMergedBrieven);
				LOG.info("Mergedocument(id = " + labFormulierenMergedBrieven.getId() + ") gegenereerd en klaar!");
				labFormulierenMergedBrieven = null;
			}
			if (voorbladerenMergedBrieven != null)
			{
				voegPrintFunctionaliteitToe(voorbladerenMergedBrieven);
				hibernateService.saveOrUpdate(voorbladerenMergedBrieven);
				LOG.info("Mergedocument(id = " + voorbladerenMergedBrieven.getId() + ") gegenereerd en klaar!");
				voorbladerenMergedBrieven = null;
			}
		}

	}

	@Override
	public void write(List<? extends Long> items) throws Exception
	{
		for (Long id : items)
		{
			CervixLabformulierAanvraag aanvraag = hibernateService.load(CervixLabformulierAanvraag.class, id);

			if (IBANValidator.getInstance().isValid(aanvraag.getHuisartsLocatie().getIban()))
			{
				volgnummerBatch++;

				controleerMergedBrieven(aanvraag);

				maakVoorbladBrief(aanvraag);

				maakLabfomulieren(aanvraag);

				aanvraag.setStatus(CervixLabformulierAanvraagStatus.AFGEDRUKT_KLAAR_OM_TE_VERSTUREN);
				aanvraag.setStatusDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(aanvraag);

				huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getAanvraagDto(aanvraag));
			}
			else
			{
				String melding = String.format("Foutief IBAN voor AGB: %s, locatie: %s", aanvraag.getHuisartsLocatie().getHuisarts().getAgbcode(),
					aanvraag.getHuisartsLocatie().getNaam());
				List<Instelling> instellingen = Collections.singletonList(aanvraag.getHuisartsLocatie().getLocatieAdres().getGbaGemeente().getScreeningOrganisatie());
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_GENEREREN_IBAN_FOUT, instellingen, null, melding, Bevolkingsonderzoek.CERVIX);
			}

		}
		if (voorbladerenMergedBrieven != null)
		{
			hibernateService.saveOrUpdate(voorbladerenMergedBrieven);
		}
		if (labFormulierenMergedBrieven != null)
		{
			hibernateService.saveOrUpdate(labFormulierenMergedBrieven);
		}
	}

	private void controleerMergedBrieven(CervixLabformulierAanvraag aanvraag)
	{
		if (labFormulierenMergedBrieven != null && labFormulierenMergedBrieven.getAantalBrieven() + aanvraag.getAantal() > MAXBRIEVENPDF)
		{
			voegPrintFunctionaliteitToe(voorbladerenMergedBrieven);
			hibernateService.saveOrUpdate(voorbladerenMergedBrieven);
			LOG.info("Mergedocument(id = " + voorbladerenMergedBrieven.getId() + ") gegenereerd en klaar!");
			voorbladerenMergedBrieven = null;

			voegPrintFunctionaliteitToe(labFormulierenMergedBrieven);
			hibernateService.saveOrUpdate(labFormulierenMergedBrieven);
			LOG.info("Mergedocument(id = " + labFormulierenMergedBrieven.getId() + ") gegenereerd en klaar!");
			labFormulierenMergedBrieven = null;
		}

		if (labFormulierenMergedBrieven == null)
		{
			if (volgnummerDocument == 0)
			{
				volgnummerDocument = 1;
			}
			else
			{
				volgnummerDocument = volgnummerBatch;
			}
			voorbladerenMergedBrieven = maakRegioMergedBrieven(BriefType.REGIO_UITSTRIJKEND_ARTS_VOORBLAD_LABFORMULIER);
			labFormulierenMergedBrieven = maakRegioMergedBrieven(BriefType.REGIO_UITSTRIJKEND_ARTS_LABFORMULIER);
		}
	}

	public void maakVoorbladBrief(CervixLabformulierAanvraag aanvraag)
	{
		FileOutputStream output = null;
		try
		{
			UploadDocument voorbladTemplate = getNieuwsteBriefDefinitie(BriefType.REGIO_UITSTRIJKEND_ARTS_VOORBLAD_LABFORMULIER);
			Document asposeDocument = mergeBrief(aanvraag, voorbladTemplate);

			File tmpFile = File.createTempFile("voorbladTmpBrief", "pdf");
			output = new FileOutputStream(tmpFile);
			asposeDocument.save(output, asposeService.getPdfSaveOptions());
			output.close();
			mergePDF(aanvraag, tmpFile, voorbladerenMergedBrieven);

			voorbladerenMergedBrieven.getBrieven().add(aanvraag.getVoorbladBrief());
			aanvraag.getVoorbladBrief().setMergedBrieven(voorbladerenMergedBrieven);
			voorbladerenMergedBrieven.setAantalBrieven(voorbladerenMergedBrieven.getAantalBrieven() + 1);
		}
		catch (Exception e)
		{
			crashMelding("Er is een onbekende fout opgetreden met het maken van een voorblad", e);
		}
	}

	public void maakLabfomulieren(CervixLabformulierAanvraag aanvraag) throws Exception
	{
		labFormulierenMergedBrieven.getBrieven().add(aanvraag.getBrief());
		int aantal = aanvraag.getAantal();
		FileOutputStream output = null;
		Document chunkDocument = null;
		try
		{
			UploadDocument labfomrulierenTemplate = getNieuwsteBriefDefinitie(BriefType.REGIO_UITSTRIJKEND_ARTS_LABFORMULIER);
			for (int i = 0; i < aantal; i++)
			{
				Document asposeDocument = mergeBrief(aanvraag, labfomrulierenTemplate);
				if (chunkDocument == null)
				{
					chunkDocument = asposeDocument;
				}
				else
				{
					chunkDocument.appendDocument(asposeDocument, ImportFormatMode.USE_DESTINATION_STYLES);
				}
			}
			labFormulierenMergedBrieven.setAantalBrieven(labFormulierenMergedBrieven.getAantalBrieven() + aantal);
			labFormulierenMergedBrieven.getBrieven().add(aanvraag.getBrief());
			aanvraag.getBrief().setMergedBrieven(labFormulierenMergedBrieven);
			File tmpFile = File.createTempFile("labformulieren", "pdf");
			output = new FileOutputStream(tmpFile);
			chunkDocument.save(output, asposeService.getPdfSaveOptions());
			output.close();
			mergePDF(aanvraag, tmpFile, labFormulierenMergedBrieven);
		}
		catch (Exception e)
		{
			crashMelding("Er is een onbekende fout opgetreden bij het maken van de labformulieren", e);
			throw e;
		}
	}

	private void voegPrintFunctionaliteitToe(CervixRegioMergedBrieven cervixRegioMergedBrieven)
	{
		FileOutputStream outputStream = null;
		try
		{
			File file = fileService.load(cervixRegioMergedBrieven.getMergedBrieven());
			PDDocument pdfBoxDocument = PDDocument.load(file);
			PDActionJavaScript javaScript = new PDActionJavaScript(JavaScriptPdfHelper.getPrintJavascript());
			pdfBoxDocument.getDocumentCatalog().setOpenAction(javaScript);
			outputStream = new FileOutputStream(file);
			pdfBoxDocument.save(outputStream);
			pdfBoxDocument.close();
		}
		catch (Exception e)
		{
			crashMelding("Er is iets misgegaan met de toevoegen van de print functionaliteit", e);
		}
		finally
		{
			if (outputStream != null)
			{
				try
				{
					outputStream.close();
				}
				catch (Exception e)
				{
					LOG.error("Er is iets misgegaan met de toevoegen van de print functionaliteit", e);
				}
			}
		}

	}

	private String voegNaamgevingAanPdf(CervixRegioMergedBrieven mergedBrieven)
	{
		String naam = "";
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
		if (mergedBrieven.getCreatieDatum() != null)
		{
			naam += sdf.format(mergedBrieven.getCreatieDatum()) + "-";
		}
		if (mergedBrieven.getScreeningOrganisatie() != null)
		{
			String soNaam = mergedBrieven.getScreeningOrganisatie().getNaam();
			soNaam = soNaam.replaceAll(" ", "_");
			naam += soNaam + "-";
		}
		if (BriefType.REGIO_UITSTRIJKEND_ARTS_LABFORMULIER == mergedBrieven.getBriefType())
		{
			naam += "Labformulieren-";
		}
		else
		{
			naam += "Oplegbrieven_labformulieren-";
		}
		naam += getVolgnummersTekst();
		return naam += ".pdf";
	}

	private String getVolgnummersTekst()
	{
		int waarde = volgnummerBatch - volgnummerDocument;
		if (waarde == 0)
		{
			return "volgnummer_" + volgnummerDocument;
		}
		else
		{
			return "volgnummers_" + volgnummerDocument + "_tot_" + volgnummerBatch;
		}
	}

	private Document mergeBrief(CervixLabformulierAanvraag aanvraag, UploadDocument document) throws Exception
	{
		File briefTemplate = fileService.load(document);
		byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
		MailMergeContext context = new MailMergeContext();
		context.putValue(MailMergeContext.CONTEXT_HA_LAB_FORM_VOLGNUMMER, volgnummerBatch);
		context.putValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS, aanvraag.getHuisartsLocatie().getHuisarts());
		context.putValue(MailMergeContext.CONTEXT_HA_LOCATIE, aanvraag.getHuisartsLocatie());
		context.putValue(MailMergeContext.CONTEXT_HA_AANTAL_FORM, aanvraag.getAantal());
		Document asposeDocument = asposeService.processDocument(briefTemplateBytes, context);
		return asposeDocument;
	}

	private CervixRegioMergedBrieven maakRegioMergedBrieven(BriefType type)
	{
		CervixRegioMergedBrieven mergedBrieven = new CervixRegioMergedBrieven();
		mergedBrieven.setBriefType(type);
		mergedBrieven.setCreatieDatum(currentDateSupplier.getDate());
		mergedBrieven.setGeprint(false);
		mergedBrieven.setScreeningOrganisatie(getScreeningOrganisatie());
		mergedBrieven.setBrieven(new ArrayList<CervixRegioBrief>());
		return mergedBrieven;
	}

	private void mergePDF(CervixLabformulierAanvraag aanvraag, File mergedPdfFile, CervixRegioMergedBrieven mergedBrieven) throws IOException, Exception
	{
		if (mergedBrieven.getMergedBrieven() == null)
		{
			LOG.info(getTechnischeLoggingMergedBriefAanmaken(mergedBrieven));
			UploadDocument uploadDocument = new UploadDocument();
			uploadDocument.setActief(Boolean.TRUE);
			uploadDocument.setContentType("application/pdf");
			uploadDocument.setNaam(voegNaamgevingAanPdf(mergedBrieven));
			uploadDocument.setFile(mergedPdfFile);

			Long fileStoreId = aanvraag.getHuisartsLocatie().getHuisarts().getId();
			fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.INSTELLING_MERGED_BRIEVEN, fileStoreId);

			mergedBrieven.setMergedBrieven(uploadDocument);
			mergedPdfFile.delete();
			LOG.info("Mergedocument(id = " + mergedBrieven.getId() + ") nieuw aangemaakt op filestore: " + uploadDocument.getPath());
		}
		else
		{
			FileOutputStream outputStream = null;
			try
			{

				UploadDocument uploadDocumentMergedBrieven = mergedBrieven.getMergedBrieven();
				File copyMergedBrievenFile = File.createTempFile("CopyMergedBrieven", ".pdf");
				File mergedBrievenFile = fileService.load(uploadDocumentMergedBrieven);
				uploadDocumentMergedBrieven.setNaam(voegNaamgevingAanPdf(mergedBrieven));
				FileUtils.copyFile(mergedBrievenFile, copyMergedBrievenFile);

				PDFMergerUtility pdfMergerUtility = new PDFMergerUtility();
				pdfMergerUtility.addSource(copyMergedBrievenFile);
				pdfMergerUtility.addSource(mergedPdfFile);
				outputStream = new FileOutputStream(mergedBrievenFile);
				pdfMergerUtility.setDestinationStream(outputStream);
				pdfMergerUtility.mergeDocuments(MemoryUsageSetting.setupMainMemoryOnly());
				fileService.saveOrUpdateUploadDocument(uploadDocumentMergedBrieven, FileStoreLocation.INSTELLING_MERGED_BRIEVEN,
					aanvraag.getHuisartsLocatie().getHuisarts().getId());
				outputStream.close();

				copyMergedBrievenFile.delete();
				mergedPdfFile.delete();
			}
			finally
			{
				if (outputStream != null)
				{
					outputStream.close();
				}
			}
		}
	}

	private UploadDocument getNieuwsteBriefDefinitie(BriefType type)
	{
		BriefDefinitie briefDefinitie = briefService.getNieuwsteBriefDefinitie(type);
		return briefDefinitie.getDocument();
	}

	private ScreeningOrganisatie getScreeningOrganisatie()
	{
		ScreeningOrganisatie screeningOrganisatie = hibernateService.load(ScreeningOrganisatie.class,
			getStepExecutionContext().getLong(LabformulierGenererenPartitioner.KEY_SCREENINGORGANISATIEID));
		return screeningOrganisatie;
	}

	protected String getTechnischeLoggingMergedBriefAanmaken(CervixRegioMergedBrieven brieven)
	{
		String tekst = "Mergedocument(id = " + brieven.getId() + ") aangemaakt voor ScreeningOrganisatie " + brieven.getScreeningOrganisatie().getNaam() + ", brieftype "
			+ brieven.getBriefType().name();
		return tekst;
	}

	protected void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		getExecutionContext().put(BatchConstants.MELDING, melding);
		getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	protected ExecutionContext getStepExecutionContext()
	{
		return stepExecution.getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		this.jobExecution = stepExecution.getJobExecution();
	}
}
