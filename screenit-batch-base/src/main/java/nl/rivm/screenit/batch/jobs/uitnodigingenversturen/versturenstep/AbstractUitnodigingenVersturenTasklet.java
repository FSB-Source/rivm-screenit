package nl.rivm.screenit.batch.jobs.uitnodigingenversturen.versturenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import lombok.extern.slf4j.Slf4j;

import nl.dm_ict.photo._358.MERGEDATA;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS.MERGEFIELD;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.util.WebservicePingUtil;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.tempuri.IUpload;
import org.tempuri.UploadRequest;

import com.aspose.words.Document;
import com.aspose.words.SaveFormat;
import com.google.common.collect.Lists;

@Slf4j
public abstract class AbstractUitnodigingenVersturenTasklet<U extends InpakbareUitnodiging<?>> implements Tasklet
{
	private static final int MINUTES_TO_WAIT_FOR_NEXT_TRY = 10;

	private static final int NR_OF_TRIES = 7;

	private static int NR_OF_RETRIES_ZIP_XML_VERZENDEN;

	private static int TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN;

	private static final String WSDL_QUESTION = "?wsdl";

	private StepExecution stepExecution;

	private JobExecution jobExecution;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private WebserviceInpakcentrumOpzettenService webserviceOpzettenService;

	@Autowired
	@Qualifier(value = "inpakCentrumEndpointUrl")
	private String inpakCentrumEndpointUrl;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AsposeService asposeService;

	private ConcurrentLinkedQueue<UploadDocument> inpakcentrumBrieven;

	private ConcurrentLinkedQueue<Long> gegenereerdeuitnodigingIds;

	private AtomicInteger volgnummer;

	private MERGEDATA mergedata;

	private String bvoAfkorting;

	private Integer onsuccesvolleVerzendPogingenGehad = 0;

	protected abstract List<Long> getUitnodigingen();

	protected abstract BriefDefinitie getBriefDefinitie(U uitnodiging);

	protected abstract void setMergedBrieven(U uitnodiging, UploadDocument uploadDocument, BriefDefinitie briefDefinitie);

	protected abstract void setGegenereerd(U uitnodiging);

	protected abstract void setMergeContext(U uitnodiging, MailMergeContext mailMergeContext);

	protected abstract FileStoreLocation getFileStoreLocation();

	protected abstract void updateCounts(U uitnodiging);

	protected abstract void logMislukt(nl.rivm.screenit.model.Client client);

	protected abstract void logMislukt(Long uitnodigingId);

	protected abstract void logNietBereikbaar(LogEvent event);

	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception
	{
		stepExecution = chunkContext.getStepContext().getStepExecution();
		jobExecution = stepExecution.getJobExecution();

		controleerAlleBrieven();
		var uitnodigingIds = getUitnodigingen();
		try
		{
			pingOrWaitForWSDL(inpakCentrumEndpointUrl.replace("/DataUpload", WSDL_QUESTION));
		}
		catch (Exception e)
		{
			return handleExceptionPingen(contribution, e);
		}

		if (!uitnodigingIds.isEmpty())
		{
			onsuccesvolleVerzendPogingenGehad = 0;
			NR_OF_RETRIES_ZIP_XML_VERZENDEN = preferenceService.getInteger(PreferenceKey.RETRIES_VERZENDEN_INPAKCENTRUM.name());
			TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN = preferenceService.getInteger(PreferenceKey.TIME_BETWEEN_RETRIES_VERZENDEN_INPAKCENTRUM.name());

			gegenereerdeuitnodigingIds = new ConcurrentLinkedQueue<>();
			inpakcentrumBrieven = new ConcurrentLinkedQueue<>();
			volgnummer = new AtomicInteger(0);
			mergedata = new MERGEDATA();
			bvoAfkorting = getBvoAfkorting();

			var aantalBeschikbareCores = Runtime.getRuntime().availableProcessors();
			var aantalThreadsVoorGenereren = Math.min(aantalBeschikbareCores * BatchConstants.AANTAL_THREADS_PER_CORE, BatchConstants.MAXIMUM_AANTAL_THREADS);
			LOG.info("Gebruik " + aantalThreadsVoorGenereren + " threads om " + uitnodigingIds.size() + " uitnodigingen te genereren.");
			var forkJoinPool = new ForkJoinPool(aantalThreadsVoorGenereren);

			var startMetGenererenTijd = System.currentTimeMillis();
			for (var uitnodigingId : uitnodigingIds)
			{
				forkJoinPool.submit(() -> OpenHibernate5Session.withCommittedTransaction().run(() ->
					genereerUitnodiging(uitnodigingId)));
			}
			forkJoinPool.shutdown();
			forkJoinPool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
			var stopMetGenererenTijd = System.currentTimeMillis();
			LOG.info("Klaar met genereren van " + gegenereerdeuitnodigingIds.size() + " uitnodigingen, dit duurde " + (stopMetGenererenTijd - startMetGenererenTijd) + "ms");
			verstuurUitnodigingen(Lists.newArrayList(gegenereerdeuitnodigingIds));
		}
		else
		{
			LOG.warn("Niets om te versturen");
		}

		return RepeatStatus.FINISHED;
	}

	protected abstract String getBvoAfkorting();

	private void controleerAlleBrieven()
	{
		if (!checkBrieven())
		{
			String message = "Niet alle brieven hebben een template geupload. Upload de bijbehorende templates.";
			IllegalStateException exception = new IllegalStateException(message);
			crashMelding(message, exception);
			throw exception;
		}
	}

	private void pingOrWaitForWSDL(String url)
	{
		int i = 0;
		if (!url.endsWith(WSDL_QUESTION))
		{
			url += WSDL_QUESTION;
		}
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");

		StringBuilder message = new StringBuilder();
		message.append("\n<br>Poging 1 (").append(format.format(currentDateSupplier.getDate())).append("): ");

		while (!WebservicePingUtil.ping(url, Arrays.asList("GetReady", "NumberOfRecords"), message))
		{
			i++;
			LogEvent event = new LogEvent();

			String melding = "Na poging " + i;
			LOG.info(melding);
			event.setMelding(melding);
			if (i == NR_OF_TRIES)
			{
				event.setLevel(Level.ERROR);
			}
			else
			{
				event.setLevel(Level.WARNING);
			}
			logNietBereikbaar(event);
			if (i < NR_OF_TRIES)
			{
				try
				{
					Thread.sleep(MINUTES_TO_WAIT_FOR_NEXT_TRY * 60 * 1000);
				}
				catch (InterruptedException e)
				{
					LOG.error("Fout in sleep", e);
					Thread.currentThread().interrupt();
				}
				message.append("\n<br>Poging ").append(i + 1).append(" (").append(format.format(currentDateSupplier.getDate())).append("): ");
			}
			else
			{
				break;
			}
		}
		if (i == NR_OF_TRIES)
		{
			Map<String, Object> parameters = new HashMap<>();
			parameters.put("organisatieType", OrganisatieType.INPAKCENTRUM);
			parameters.put("actief", Boolean.TRUE);
			List<Instelling> inpakcentra = hibernateService.getByParameters(Instelling.class, parameters);
			List<String> emailadressen = new ArrayList<>();
			for (Instelling inpakcentrum : inpakcentra)
			{
				if (StringUtils.isNotBlank(inpakcentrum.getEmail()))
				{
					emailadressen.add(inpakcentrum.getEmail());
				}
			}
			emailadressen.addAll(getEmails(preferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name())));

			message.insert(0, "Na " + NR_OF_TRIES + " pogingen in het afgelopen uur is de Inpakcentrum WSDL " + url + " niet bereikbaar geweest. ");
			mailService.queueMailAanProfessional(String.join(";", emailadressen), "Inpakcentrum WSDL niet bereikbaar", message.toString(), MailPriority.HIGH);
			throw new IllegalStateException(message.toString());
		}
	}

	private RepeatStatus handleExceptionPingen(StepContribution contribution, Exception e)
	{
		LOG.error("Pingen levert niets op ", e);
		stepExecution.setExitStatus(ExitStatus.FAILED);
		stepExecution.setStatus(BatchStatus.FAILED);
		stepExecution.getJobExecution().addFailureException(e);
		contribution.setExitStatus(ExitStatus.FAILED);
		return RepeatStatus.FINISHED;
	}

	private void genereerUitnodiging(Long uitnodigingId)
	{
		U uitnodiging = getUitnodigingById(uitnodigingId);
		try
		{
			if (geenUitzonderingGevonden(uitnodiging))
			{
				beforeProcessUitnodiging(uitnodiging);
				LOG.trace("Geneer uitnodiging voor uitnodigingId: " + uitnodigingId);
				var inpakcentrumUitnodiging = new UITNODIGING();
				inpakcentrumUitnodiging.setID(uitnodiging.getUitnodigingsId());
				inpakcentrumUitnodiging.setMERGEFIELDS(new MERGEFIELDS());

				var briefDefinitie = getBriefDefinitie(uitnodiging);
				var client = uitnodiging.getScreeningRonde().getDossier().getClient();
				var briefActie = projectService.getProjectBriefActie(client, briefDefinitie.getBriefType());

				File briefTemplate;
				if (briefActie != null)
				{
					var inpakcentrumTemplateNaam = maakInpakcentrumTemplateNaam(briefActie);
					inpakcentrumUitnodiging.setTEMPLATE(inpakcentrumTemplateNaam);
					uitnodiging.setTemplateNaam(briefActie.getProject().getNaam() + ": " + briefActie.getDocument().getNaam());
					briefTemplate = uploadDocumentService.load(briefActie.getDocument());
				}
				else
				{
					var inpakcentrumTemplateNaam = maakInpakcentrumTemplateNaam(briefDefinitie);
					inpakcentrumUitnodiging.setTEMPLATE(inpakcentrumTemplateNaam);
					var document = briefDefinitie.getDocument();
					uitnodiging.setTemplateNaam(document.getNaam());
					briefTemplate = uploadDocumentService.load(document);
				}

				byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
				if (briefActie != null)
				{
					var templateDocument = new Document(new ByteArrayInputStream(briefTemplateBytes));
					var context = new MailMergeContext();
					if (projectService.addVragenlijstAanTemplate(context, templateDocument, briefActie, null))
					{

						ByteArrayOutputStream baos = new ByteArrayOutputStream();
						templateDocument.save(baos, SaveFormat.DOCX);
						briefTemplateBytes = baos.toByteArray();
					}
				}
				var mailMergeContext = new MailMergeContext();
				mailMergeContext.setClient(client);
				setMergeContext(uitnodiging, mailMergeContext);

				var document = asposeService.processDocument(briefTemplateBytes, mailMergeContext);
				var uitnodigingFile = briefService.genereerPdf(document, "uitnodiging", false);

				int uitnodigingVolgnummer = volgnummer.getAndIncrement();
				UploadDocument uploadDocument = new UploadDocument();
				uploadDocument.setActief(Boolean.TRUE);
				uploadDocument.setContentType("application/pdf");
				uploadDocument.setNaam(maakPdfNaam(uitnodiging, uitnodigingVolgnummer));
				uploadDocument.setFile(uitnodigingFile);

				var timestamp = currentDateSupplier.getDate().getTime();
				uploadDocumentService.saveOrUpdate(uploadDocument, getFileStoreLocation(), timestamp, true);

				inpakcentrumBrieven.add(uploadDocument);

				setMergedBrieven(uitnodiging, uploadDocument, briefDefinitie);

				setGegenereerd(uitnodiging);

				vulMetaData(inpakcentrumUitnodiging, briefActie, mailMergeContext, uitnodigingVolgnummer, uploadDocument);

				updateCounts(uitnodiging);
				uitnodiging.setVerstuurd(true);
				hibernateService.saveOrUpdate(uitnodiging);
				gegenereerdeuitnodigingIds.add(uitnodiging.getId());
			}
		}
		catch (Exception e)
		{
			if (uitnodiging == null)
			{
				LOG.warn("Uitnodiging overgeslagen door een null uitnodiging", e);
				logMislukt(uitnodigingId);
			}
			else
			{
				nl.rivm.screenit.model.Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
				LOG.warn("Client (id: '{}') overgeslagen door een exception", client.getId(), e);
				logMislukt(client);
			}
		}
	}

	protected void vulMetaData(UITNODIGING inpakcentrumUitnodiging, ProjectBriefActie briefActie, MailMergeContext mailMergeContext, int uitnodigingVolgnummer,
		UploadDocument uploadDocument)
	{
		var mergefieldContainer = inpakcentrumUitnodiging.getMERGEFIELDS().getMERGEFIELD();
		var teSturenMergefields = Arrays.asList(MergeField.SO_ID, MergeField.CLIENT_NAAM, MergeField.CLIENT_ADRES, MergeField.CLIENT_POSTCODE,
			MergeField.CLIENT_WOONPLAATS, MergeField.KIX_CLIENT);

		for (var mergeField : teSturenMergefields)
		{
			if (mergeField.naarInpakcentrum())
			{
				Object value = mergeField.getValue(mailMergeContext);
				addMergeFieldValue(mergefieldContainer, mergeField.getFieldName(), value != null ? value.toString() : "");
			}
		}

		addMergeFieldValue(mergefieldContainer, "_VOLGNUMMER", Integer.toString(uitnodigingVolgnummer));
		addMergeFieldValue(mergefieldContainer, "_BVO", bvoAfkorting);
		addMergeFieldValue(mergefieldContainer, "_PROJECT", briefActie != null ? "P" + briefActie.getProject().getId() + ":" + briefActie.getProject().getNaam() : "");
		addMergeFieldValue(mergefieldContainer, "_PDF", uploadDocument.getNaam());
		updateMergeData(inpakcentrumUitnodiging);
	}

	protected void addMergeFieldValue(List<MERGEFIELD> mergefieldContainer, String key, String value)
	{
		var mergeField = new MERGEFIELD();
		mergeField.setNAME(key);
		mergeField.setVALUE(value);
		mergefieldContainer.add(mergeField);
	}

	protected void beforeProcessUitnodiging(U uitnodiging)
	{
	}

	protected abstract U getUitnodigingById(Long uitnodigingId);

	private void verstuurUitnodigingen(List<Long> uitnodigingIds) throws JAXBException, IOException
	{
		if (!uitnodigingIds.isEmpty())
		{
			LOG.info("Webservice opzetten");
			var jaxbContext = JAXBContext.newInstance(MERGEDATA.class);
			var marshaller = jaxbContext.createMarshaller();
			var byteArrayOutputStream = new ByteArrayOutputStream();
			marshaller.marshal(mergedata, byteArrayOutputStream);

			var upload = webserviceOpzettenService.initialiseerWebserviceInpakcentrum();

			LOG.info("Start versturen, uitnodigingIds size: " + uitnodigingIds.size() + ", id van eerste uitnodiging: " + uitnodigingIds.get(0));
			var dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
			var uploadRequest = new UploadRequest();
			uploadRequest.setStream(byteArrayOutputStream.toByteArray());

			var versturenGeslaagd = verstuurXml(uitnodigingIds.size(), upload, dateFormat, uploadRequest);

			if (versturenGeslaagd)
			{
				var zips = zipInpakcentrumBrieven(bvoAfkorting);
				versturenGeslaagd = verstuurZips(upload, zips);
			}

			versturenGeslaagd = upload.getReady(versturenGeslaagd);

			LOG.info("Alles verstuurd met resultaat: " + versturenGeslaagd);
			if (versturenGeslaagd)
			{
				setUitnodigingVersturenTijd(uitnodigingIds);
			}
			else
			{
				crashMelding("Niet alles is correct verstuurd naar het inpakcentrum. Neem contact op met de helpdesk", null);
			}
		}
		else
		{
			LOG.warn("Niets om te versturen. Client(en) overgeslagen door onbekende/onvolledige retouradres.");
		}
	}

	private boolean verstuurXml(int aantalUitnodigingen, IUpload upload, SimpleDateFormat dateFormat, UploadRequest uploadRequest)
	{
		boolean versturenGeslaagd;

		do
		{
			versturenGeslaagd = upload
				.upload(uploadRequest, "xml", bvoAfkorting + "_mergedata" + dateFormat.format(currentDateSupplier.getDate()) + ".xml",
					aantalUitnodigingen)
				.isUploadSucceeded();
		}
		while (bepaalMagNogmaalsProberen(versturenGeslaagd));

		return versturenGeslaagd;
	}

	private boolean verstuurZips(IUpload upload, Set<File> zips)
	{
		var versturenGeslaagd = true;
		for (var zip : zips)
		{
			LOG.info(zip.getName() + " grootte is: " + zip.length() + " bytes");
			versturenGeslaagd &= newUploadRequest(upload, zip, zip.getName());
			FileUtils.deleteQuietly(zip);
		}
		return versturenGeslaagd;
	}

	private boolean bepaalMagNogmaalsProberen(boolean versturenGeslaagd)
	{
		if (versturenGeslaagd || onsuccesvolleVerzendPogingenGehad == NR_OF_RETRIES_ZIP_XML_VERZENDEN)
		{
			onsuccesvolleVerzendPogingenGehad = 0;
			return false;
		}

		onsuccesvolleVerzendPogingenGehad++;
		try
		{
			LOG.info("Retry nodig, wacht {} seconden", TimeUnit.MILLISECONDS.toSeconds(TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN));
			Thread.sleep(TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN);
		}
		catch (InterruptedException e)
		{
			LOG.error("Fout in sleep", e);
			Thread.currentThread().interrupt();
		}

		return true;
	}

	protected abstract void setUitnodigingVersturenTijd(List<Long> uitnodigingIds);

	private Set<File> zipInpakcentrumBrieven(String bvoAfkorting) throws IOException
	{
		var dateTime = LocalDateTime.now();
		var datumTijd = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(dateTime);
		var baseZipNaam = bvoAfkorting.toLowerCase() + "_" + datumTijd;

		var maxBestandsGrootte = preferenceService.getInteger(PreferenceKey.INTERNAL_MAX_GROOTTE_ZIP.name());

		return ZipUtil.maakZips(Lists.newArrayList(inpakcentrumBrieven), baseZipNaam, maxBestandsGrootte);
	}

	protected abstract boolean geenUitzonderingGevonden(U uitnodiging);

	private String maakPdfNaam(InpakbareUitnodiging<?> uitnodiging, int volgnummer)
	{
		var dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
		return String.format("%s_%s_%s.pdf", dateFormat.format(currentDateSupplier.getDate()), volgnummer, uitnodiging.getUitnodigingsId());
	}

	private String maakInpakcentrumTemplateNaam(BriefDefinitie briefDefinitie)
	{
		var dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
		return briefDefinitie.getBriefType().name() + "_" + dateFormat.format(briefDefinitie.getLaatstGewijzigd());
	}

	private String maakInpakcentrumTemplateNaam(ProjectBriefActie briefActie)
	{
		var dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
		String templateNaam = briefActie.getBriefType().name();
		if (briefActie.getProject() != null)
		{
			templateNaam += "_P" + briefActie.getProject().getId() + "_";
		}
		templateNaam += dateFormat.format(briefActie.getLaatstGewijzigd());
		return templateNaam;
	}

	private boolean newUploadRequest(IUpload upload, File file, String fileName)
	{
		if (file == null)
		{

			LOG.warn("UploadDocument was null wordt niet verstuurd naar inpakcentrum! fileName: " + fileName);
			return true;
		}
		var extensie = FilenameUtils.getExtension(fileName);
		if (StringUtils.isBlank(extensie))
		{
			LOG.error("Kon geen extensie bepaald worden bij de fileName: " + fileName);
			return false;
		}
		LOG.info("File met naam '" + fileName + "' wordt naar inpakcentrum verstuurd");

		boolean verzendenGeslaagd;

		do
		{
			try
			{
				var uploadRequest = new UploadRequest();
				byte[] template = FileUtils.readFileToByteArray(file);
				uploadRequest.setStream(template);
				verzendenGeslaagd = upload.upload(uploadRequest, extensie, fileName, 1).isUploadSucceeded();
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden met uploaden naar het inpakcentrum", e);
				verzendenGeslaagd = false;
			}
		}
		while (bepaalMagNogmaalsProberen(verzendenGeslaagd));

		return verzendenGeslaagd;
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	private List<String> getEmails(String emailreeks)
	{
		List<String> emails = new ArrayList<>();
		if (emailreeks != null)
		{
			if (emailreeks.contains(";"))
			{
				emails = Arrays.asList(emailreeks.split(";"));
			}
			else if (emailreeks.contains(","))
			{
				emails = Arrays.asList(emailreeks.split(","));
			}
			else
			{
				emails.add(emailreeks);
			}
		}
		return emails;
	}

	protected boolean checkBrieven()
	{
		for (var briefType : BriefType.values())
		{
			if (OrganisatieType.INPAKCENTRUM == briefType.getVerzendendeOrganisatieType() && briefService.getNieuwsteBriefDefinitie(briefType) == null)
			{
				return false;
			}

		}
		return true;
	}

	private synchronized void updateMergeData(UITNODIGING inpakcentrumUitnodiging)
	{
		mergedata.getUITNODIGING().add(inpakcentrumUitnodiging);
	}

	private void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		if (!getExecutionContext().containsKey(BatchConstants.MELDING) || !Level.ERROR.equals(getExecutionContext().get(BatchConstants.LEVEL)))
		{
			getExecutionContext().put(BatchConstants.MELDING, melding);
			getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
		}
	}
}
