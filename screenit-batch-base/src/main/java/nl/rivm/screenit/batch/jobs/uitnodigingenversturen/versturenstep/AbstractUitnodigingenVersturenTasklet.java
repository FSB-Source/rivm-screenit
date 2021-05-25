package nl.rivm.screenit.batch.jobs.uitnodigingenversturen.versturenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import javax.xml.bind.Marshaller;

import nl.dm_ict.photo._358.MERGEDATA;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS.MERGEFIELD;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.util.WebservicePingUtil;
import nl.rivm.screenit.dao.BaseBriefDao;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.MailService.MailPriority;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.neethi.Policy;
import org.apache.neethi.PolicyComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

public abstract class AbstractUitnodigingenVersturenTasklet<U extends InpakbareUitnodiging<?>> implements Tasklet
{
	private static final int MINUTES_TO_WAIT_FOR_NEXT_TRY = 10;

	private static final int NR_OF_TRIES = 7;

	private static final String WSDL_QUESTION = "?wsdl";

	private static final Logger LOG = LoggerFactory.getLogger(AbstractUitnodigingenVersturenTasklet.class);

	private StepExecution stepExecution;

	private JobExecution jobExecution;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefDao briefDao;

	@Autowired
	private FileService fileService;

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
	private ProjectService projectService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AsposeService asposeService;

	private ConcurrentLinkedQueue<UploadDocument> minigripBrieven;

	private ConcurrentLinkedQueue<Long> gegenereerdeuitnodigingIds;

	private AtomicInteger volgnummer;

	private MERGEDATA mergedata;

	private String bvoAfkorting;

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
		this.stepExecution = chunkContext.getStepContext().getStepExecution();
		this.jobExecution = stepExecution.getJobExecution();

		controleerAlleBrieven();
		List<Long> uitnodigingIds = getUitnodigingen();
		try
		{
			pingOrWaitForWSDL(inpakCentrumEndpointUrl.replace("/DataUpload", WSDL_QUESTION));
		}
		catch (Exception e)
		{
			return handleExceptionPingen(contribution, e);
		}

		if (uitnodigingIds.size() > 0)
		{
			gegenereerdeuitnodigingIds = new ConcurrentLinkedQueue<>();
			minigripBrieven = new ConcurrentLinkedQueue<>();
			volgnummer = new AtomicInteger(0);
			mergedata = new MERGEDATA();
			bvoAfkorting = getBvoAfkorting();

			int aantalBeschikbareCores = Runtime.getRuntime().availableProcessors();
			int aantalThreadsVoorGenereren = Math.min(aantalBeschikbareCores * BatchConstants.AANTAL_THREADS_PER_CORE, BatchConstants.MAXIMUM_AANTAL_THREADS);
			LOG.info("Gebruik " + aantalThreadsVoorGenereren + " threads om " + uitnodigingIds.size() + " uitnodigingen te genereren.");
			ForkJoinPool forkJoinPool = new ForkJoinPool(aantalThreadsVoorGenereren);

			long startMetGenererenTijd = System.currentTimeMillis();
			for (Long uitnodigingId : uitnodigingIds)
			{
				forkJoinPool.submit(() -> OpenHibernate5Session.withCommittedTransaction().run(() -> {
					genereerUitnodiging(uitnodigingId);
				}));
			}
			forkJoinPool.shutdown();
			forkJoinPool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
			long stopMetGenererenTijd = System.currentTimeMillis();
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
		message.append("\n<br>Poging 1 (" + format.format(currentDateSupplier.getDate()) + "): ");

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
				}
				message.append("\n<br>Poging " + (i + 1) + " (" + format.format(currentDateSupplier.getDate()) + "): ");
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
				if (StringUtils.containsIgnoreCase(inpakcentrum.getNaam(), "minigrip") && StringUtils.isNotBlank(inpakcentrum.getEmail()))
				{
					emailadressen.add(inpakcentrum.getEmail());
				}
			}
			emailadressen.addAll(getEmails(preferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name())));

			message.insert(0, "Na " + NR_OF_TRIES + " pogingen in het afgelopen uur is de MiniGrip WSDL " + url + " niet bereikbaar geweest. ");
			mailService.sendEmail(emailadressen.toArray(new String[] {}), "MiniGrip WSDL niet bereikbaar", message.toString(), MailPriority.HIGH);
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
				LOG.trace("Geneer uitnodiging voor uitnodigingId: " + uitnodigingId);
				UITNODIGING minigripUitnodiging = new UITNODIGING();
				minigripUitnodiging.setID(uitnodiging.getUitnodigingsId());
				minigripUitnodiging.setMERGEFIELDS(new MERGEFIELDS());

				BriefDefinitie briefDefinitie = getBriefDefinitie(uitnodiging);
				nl.rivm.screenit.model.Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
				ProjectBriefActie briefActie = projectService.getProjectBriefActie(client, briefDefinitie.getBriefType());

				File briefTemplate;
				if (briefActie != null)
				{
					String minigripTemplateNaam = maakMinigripTemplateNaam(briefActie);
					minigripUitnodiging.setTEMPLATE(minigripTemplateNaam);
					uitnodiging.setTemplateNaam(briefActie.getProject().getNaam() + ": " + briefActie.getDocument().getNaam());
					briefTemplate = fileService.load(briefActie.getDocument());
				}
				else
				{
					String minigripTemplateNaam = maakMinigripTemplateNaam(briefDefinitie);
					minigripUitnodiging.setTEMPLATE(minigripTemplateNaam);
					UploadDocument document = briefDefinitie.getDocument();
					uitnodiging.setTemplateNaam(document.getNaam());
					briefTemplate = fileService.load(document);
				}

				MailMergeContext mailMergeContext = new MailMergeContext();
				mailMergeContext.setClient(client);
				setMergeContext(uitnodiging, mailMergeContext);

				byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
				if (briefActie != null)
				{
					Document templateDocument = new Document(new ByteArrayInputStream(briefTemplateBytes));
					MailMergeContext context = new MailMergeContext();
					if (projectService.addVragenlijstAanTemplate(context, templateDocument, briefActie, null))
					{

						ByteArrayOutputStream baos = new ByteArrayOutputStream();
						templateDocument.save(baos, SaveFormat.DOCX);
						briefTemplateBytes = baos.toByteArray();
					}
				}

				Document document = asposeService.processDocument(briefTemplateBytes, mailMergeContext);
				File uitnodigingFile = briefService.genereerPdf(document, "uitnodiging", false);

				int uitnodigingVolgnummer = volgnummer.getAndIncrement();
				UploadDocument uploadDocument = new UploadDocument();
				uploadDocument.setActief(Boolean.TRUE);
				uploadDocument.setContentType("application/pdf");
				uploadDocument.setNaam(maakPdfNaam(uitnodiging, uitnodigingVolgnummer));
				uploadDocument.setFile(uitnodigingFile);

				Long timestamp = currentDateSupplier.getDate().getTime();
				fileService.saveOrUpdateUploadDocument(uploadDocument, getFileStoreLocation(), timestamp, true);

				minigripBrieven.add(uploadDocument);

				setMergedBrieven(uitnodiging, uploadDocument, briefDefinitie);

				setGegenereerd(uitnodiging);

				List<MERGEFIELD> mergefieldContainer = minigripUitnodiging.getMERGEFIELDS().getMERGEFIELD();
				List<MergeField> teSturenMergefields = Arrays.asList(MergeField.SO_ID, MergeField.CLIENT_NAAM, MergeField.CLIENT_ADRES, MergeField.CLIENT_POSTCODE,
					MergeField.CLIENT_WOONPLAATS, MergeField.KIX_CLIENT);

				for (MergeField mergeField : teSturenMergefields)
				{
					if (mergeField.naarMinigrip())
					{
						Object value = mergeField.getValue(mailMergeContext);
						MERGEFIELD xmlMergefield = new MERGEFIELD();
						xmlMergefield.setNAME(mergeField.getFieldName());

						if (value != null)
						{
							xmlMergefield.setVALUE(value.toString());
						}
						else
						{
							xmlMergefield.setVALUE("");
						}

						mergefieldContainer.add(xmlMergefield);
					}
				}

				MERGEFIELD volgnummerMergeField = new MERGEFIELD();
				volgnummerMergeField.setNAME("_VOLGNUMMER");
				volgnummerMergeField.setVALUE(Integer.toString(uitnodigingVolgnummer));
				mergefieldContainer.add(volgnummerMergeField);

				MERGEFIELD bvoMergeField = new MERGEFIELD();
				bvoMergeField.setNAME("_BVO");
				bvoMergeField.setVALUE(bvoAfkorting);
				mergefieldContainer.add(bvoMergeField);

				MERGEFIELD projectMergeField = new MERGEFIELD();
				projectMergeField.setNAME("_PROJECT");
				if (briefActie != null)
				{
					projectMergeField.setVALUE("P" + briefActie.getProject().getId() + ":" + briefActie.getProject().getNaam());
				}
				else
				{
					projectMergeField.setVALUE("");
				}
				mergefieldContainer.add(projectMergeField);

				MERGEFIELD pdfNaamMergeField = new MERGEFIELD();
				pdfNaamMergeField.setNAME("_PDF");
				pdfNaamMergeField.setVALUE(uploadDocument.getNaam());
				mergefieldContainer.add(pdfNaamMergeField);
				updateMergeData(minigripUitnodiging);

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
				LOG.warn("Client overgeslagen door een exception, clientId: " + client.getId(), e);
				logMislukt(client);
			}
		}
	}

	protected abstract U getUitnodigingById(Long uitnodigingId);

	private void verstuurUitnodigingen(List<Long> uitnodigingIds) throws JAXBException, IOException
	{
		if (uitnodigingIds.size() > 0)
		{
			LOG.info("Webservice opzetten");
			JAXBContext jaxbContext = JAXBContext.newInstance(MERGEDATA.class);
			Marshaller marshaller = jaxbContext.createMarshaller();
			ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
			marshaller.marshal(mergedata, byteArrayOutputStream);

			IUpload upload = webserviceOpzettenService.initialiseerWebserviceInpakcentrum();

			LOG.info("Start versturen, uitnodigingIds size: " + uitnodigingIds.size() + ", id van eerste uitnodiging: " + uitnodigingIds.get(0));
			boolean result = true;

			SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
			UploadRequest uploadRequest = new UploadRequest();
			uploadRequest.setStream(byteArrayOutputStream.toByteArray());
			result &= upload
				.upload(uploadRequest, "xml", bvoAfkorting + "_mergedata" + dateFormat.format(currentDateSupplier.getDate()) + ".xml",
					uitnodigingIds.size())
				.isUploadSucceeded();

			Set<File> zips = zipMinigripBrieven(bvoAfkorting);

			result = verstuurZips(upload, result, zips);

			result = upload.getReady(result);

			LOG.info("Alles verstuurd met resultaat: " + result);
			if (result)
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

	protected abstract void setUitnodigingVersturenTijd(List<Long> uitnodigingIds);

	private boolean verstuurZips(IUpload upload, boolean result, Set<File> zips)
	{
		for (File zip : zips)
		{
			LOG.info(zip.getName() + " grootte is: " + zip.length() + " bytes");
			result &= newUploadRequest(upload, zip, zip.getName());
			FileUtils.deleteQuietly(zip);
		}
		return result;
	}

	private Set<File> zipMinigripBrieven(String bvoAfkorting) throws IOException
	{
		LocalDateTime dateTime = LocalDateTime.now();
		String datumTijd = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(dateTime);
		String baseZipNaam = bvoAfkorting.toLowerCase() + "_" + datumTijd;

		int maxBestandsGrootte = preferenceService.getInteger(PreferenceKey.INTERNAL_MAX_GROOTTE_ZIP.name());

		return ZipUtil.maakZips(Lists.newArrayList(minigripBrieven), baseZipNaam, maxBestandsGrootte);
	}

	protected abstract boolean geenUitzonderingGevonden(U uitnodiging);

	private String maakPdfNaam(InpakbareUitnodiging<?> uitnodiging, int volgnummer)
	{
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
		return String.format("%s_%s_%s.pdf", dateFormat.format(currentDateSupplier.getDate()), volgnummer, uitnodiging.getUitnodigingsId());
	}

	private String maakMinigripTemplateNaam(BriefDefinitie briefDefinitie)
	{
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
		String templateNaam = briefDefinitie.getBriefType().name() + "_" + dateFormat.format(briefDefinitie.getLaatstGewijzigd());
		return templateNaam;
	}

	private String maakMinigripTemplateNaam(ProjectBriefActie briefActie)
	{
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
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

			LOG.warn("UploadDocument was null wordt niet verstuurd naar minigrip! fileName: " + fileName);
			return true;
		}
		String extensie = FilenameUtils.getExtension(fileName);
		if (StringUtils.isBlank(extensie))
		{
			LOG.error("Kon geen extensie bepaald worden bij de fileName: " + fileName);
			return false;
		}
		LOG.info("File met naam '" + fileName + "' wordt naar inpakcentrum verstuurd");
		try
		{
			UploadRequest uploadRequest = new UploadRequest();
			byte[] template = FileUtils.readFileToByteArray(file);
			uploadRequest.setStream(template);
			return upload.upload(uploadRequest, extensie, fileName, 1).isUploadSucceeded();
		}
		catch (Exception e)
		{
			LOG.error("Er is een probleem opgetreden met uploaden naar het inpakcentrum", e);
			return false;
		}
	}

	protected ExecutionContext getExecutionContext()
	{
		return this.jobExecution.getExecutionContext();
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
		for (BriefType briefType : BriefType.values())
		{
			if (OrganisatieType.INPAKCENTRUM == briefType.getVerzendendeOrganisatieType() && briefDao.getNieuwsteBriefDefinitie(briefType) == null)
			{
				return false;
			}

		}
		return true;
	}

	private synchronized void updateMergeData(UITNODIGING minigripUitnodiging)
	{
		mergedata.getUITNODIGING().add(minigripUitnodiging);
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

	private class HttpPolicy extends Policy
	{
		@Override
		public void addPolicyComponent(PolicyComponent component)
		{

		}

		@Override
		public void addPolicyComponents(List<? extends PolicyComponent> components)
		{

		}

	}
}
