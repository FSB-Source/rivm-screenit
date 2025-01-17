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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.factory.algemeen.BriefFactory;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.algemeen.BriefDefinitieRepository;
import nl.rivm.screenit.repository.algemeen.ClientBriefRepository;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.JavaScriptPdfHelper;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.io.FileUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.interactive.action.PDActionJavaScript;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;
import com.aspose.words.PdfSaveOptions;

import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftClient;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftOngegeneerdeBrieven;

@Slf4j
@Service
public class BaseBriefServiceImpl implements BaseBriefService
{

	private static final int BYTES_TO_MBS = 1024 * 1024;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private LogService logService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private BriefDefinitieRepository briefDefinitieRepository;

	@Autowired
	private ClientBriefRepository clientBriefRepository;

	@Autowired
	private BriefFactory briefFactory;

	@Override
	public BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType)
	{
		return briefDefinitieRepository.findFirstByBriefTypeOrderByLaatstGewijzigdDesc(briefType);
	}

	@Override
	public List<BriefDefinitie> getBriefDefinities(BvoZoekCriteria criteria, Comparator<BriefType> comparator)
	{
		var result = new ArrayList<BriefDefinitie>();
		var bevolkingsonderzoeken = criteria.getBevolkingsonderzoeken();
		var briefTypes = BriefType.getBriefTypes(Boolean.TRUE.equals(criteria.getExactMatch()),
			bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[0]));
		briefTypes.sort(comparator);
		for (var briefType : briefTypes)
		{

			var briefDefinitiesVanDitBriefType = new ArrayList<BriefDefinitie>();
			int eersteOngebruikteVolgnummer = 1;
			var briefDefinities = briefDefinitieRepository.findByBriefTypeOrderByLaatstGewijzigdAsc(briefType);

			if (briefDefinities != null)
			{
				for (var briefDefinitie : briefDefinities)
				{
					briefDefinitie.setVolgnummer(eersteOngebruikteVolgnummer++);
					if (!briefDefinitiesVanDitBriefType.isEmpty())
					{ 
						briefDefinitiesVanDitBriefType.get(briefDefinitiesVanDitBriefType.size() - 1).setGeldigTot(briefDefinitie.getLaatstGewijzigd());
					}
					briefDefinitiesVanDitBriefType.add(briefDefinitie);
				}
			}

			if (briefDefinitiesVanDitBriefType.isEmpty())
			{ 
				var legeBriefDefinitie = new BriefDefinitie();
				legeBriefDefinitie.setBriefType(briefType);
				legeBriefDefinitie.setVolgnummer(1);
				briefDefinitiesVanDitBriefType.add(legeBriefDefinitie);
			}

			for (int i = briefDefinitiesVanDitBriefType.size() - 1; i >= 0; i--)
			{
				result.add(briefDefinitiesVanDitBriefType.get(i));
			}
		}
		return result;
	}

	@Override
	@Transactional
	public void saveBriefDefinitie(BriefDefinitie nieuweBriefDefinitie, File uploadFile, String contentType, String filename) throws IOException
	{

		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(contentType);
		uploadDocument.setFile(uploadFile);
		uploadDocument.setNaam(filename);

		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.BRIEF_TEMPLATES);

		nieuweBriefDefinitie.setDocument(uploadDocument);
		nieuweBriefDefinitie.setLaatstGewijzigd(currentDateSupplier.getDate());

		hibernateService.save(nieuweBriefDefinitie);

		logService.logGebeurtenis(LogGebeurtenis.BRIEF_TOEGEVOEGD, nieuweBriefDefinitie.getUploader(),
			"Brief geupload: " + nieuweBriefDefinitie.getDocument().getNaam() + ", Type: " + nieuweBriefDefinitie.getBriefType().getWeergaveNaam(),
			nieuweBriefDefinitie.getBriefType().getOnderzoeken());
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie, ProjectBrief origineleBrief)
	{
		return briefFactory.maakProjectBrief(pClient, actie, origineleBrief);
	}

	@Override
	public <B extends ClientBrief<?, ?, ?>> boolean clientHeeftOngegenereerdeBriefVanType(BriefType type, Client client, Class<B> briefClass)
	{
		var specification = heeftOngegeneerdeBrieven(client, briefClass).and(heeftBriefType(type));
		var repository = briefFactory.getBriefTypeRepository(briefClass);
		return repository.exists(specification);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date)
	{
		return briefFactory.maakBezwaarBrief(client, type, date);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date, boolean vragenOmHandtekening)
	{
		return briefFactory.maakBezwaarBrief(client, type, date, vragenOmHandtekening);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type)
	{
		return briefFactory.maakAlgemeneBrief(client, type);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment, boolean vervangendeProjectBrief)
	{
		return briefFactory.maakBvoBrief(afmelding, type, creatieMoment, vervangendeProjectBrief);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment)
	{
		return briefFactory.maakBvoBrief(afmelding, type, creatieMoment, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type)
	{
		return briefFactory.maakBvoBrief(ronde, type, null, false, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, boolean gegenereerd)
	{
		return briefFactory.maakBvoBrief(ronde, type, null, gegenereerd, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment)
	{
		return briefFactory.maakBvoBrief(ronde, type, creatieMoment, false, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment, boolean gegenereerd,
		boolean vervangendeProjectBrief)
	{
		return briefFactory.maakBvoBrief(ronde, type, creatieMoment, gegenereerd, vervangendeProjectBrief);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts)
	{
		return briefFactory.maakRegioBrief(so, type, date, arts);
	}

	@Override
	public void completePdf(MergedBrieven<?> mergedBrieven)
	{
		var file = uploadDocumentService.load(mergedBrieven.getMergedBrieven());

		try
		{
			var copy = Files.copy(file.toPath(), Paths.get(file.toPath() + "-copy")).toFile();
			var javaScript = new PDActionJavaScript(JavaScriptPdfHelper.getPrintJavascript());
			try (var document = Loader.loadPDF(copy); var outputStream = new FileOutputStream(file))
			{
				document.getDocumentCatalog().setOpenAction(javaScript);
				document.save(outputStream);
				LOG.info("Mergedocument(id = " + mergedBrieven.getId() + ") gegenereerd en klaar!");
			}
			finally
			{
				copy.delete();
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij toevoegen van javascript in PDF (voor automatische printpopup)", e);
			throw new IllegalStateException("Fout bij toevoegen van javascript in PDF (voor automatische printpopup)");
		}
	}

	@Override
	@Transactional
	public <B extends Brief, MB extends MergedBrieven<?>> void createOrAddMergedBrieven(List<? extends B> chunkItems, IBrievenGeneratorHelper<B, MB> briefGenerator)
		throws Exception
	{
		try
		{
			MB mergedBrieven = briefGenerator.getMergedBrieven();
			IDocument documentDefinitie = briefGenerator.getDocumentDefinitie();
			UploadDocument document = documentDefinitie.getDocument();
			Document chunkDocument = null;
			boolean brievenAanwezig = false;
			List<Brief> succesvolleBrieven = new ArrayList<>();
			for (B brief : chunkItems)
			{
				try
				{

					Client client = getClientFromBrief(brief);
					if (client != null && !AdresUtil.isVolledigAdresVoorInpakcentrum(client))
					{
						Adres clientAdres = AdresUtil.getAdres(client.getPersoon(), currentDateSupplier.getLocalDate());
						String onvolledigAdresMelding = "De cliënt heeft een onvolledig adres, dit is geconstateerd bij het aanmaken van: " + brief.getBriefType()
							+ ". De volgende gegevens ontbreken: " + AdresUtil.bepaalMissendeAdresgegevensString(clientAdres) + ".";
						int dagen = simplePreferenceService.getInteger(PreferenceKey.INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES.name());
						if (logService.heeftGeenBestaandeLogregelBinnenPeriode(List.of(briefGenerator.getOnvolledigAdresLogGebeurtenis()), client.getPersoon().getBsn(),
							onvolledigAdresMelding, dagen))
						{
							List<Instelling> organisaties = new ArrayList<>();
							organisaties.add(hibernateService.loadAll(Rivm.class).get(0));
							if (mergedBrieven.getScreeningOrganisatie() != null)
							{
								organisaties.add(mergedBrieven.getScreeningOrganisatie());
							}
							logService.logGebeurtenis(briefGenerator.getOnvolledigAdresLogGebeurtenis(), organisaties, client, onvolledigAdresMelding,
								briefGenerator.getBevolkingsonderzoeken());
						}
					}
					else
					{
						chunkDocument = appendDocument(document, brief, chunkDocument, briefGenerator);

						succesvolleBrieven.add(brief);
						setBriefGegenereerdInfo(brief, documentDefinitie);
						brievenAanwezig = true;
					}
				}
				catch (Exception e)
				{
					LOG.error("Error bij aanmaken brief (brieftype: {}, briefId: {})", brief.getBriefType(), brief.getId(), e);
					Client client = getClientFromBrief(brief);
					var dashboardOrganisaties = new ArrayList<Instelling>();
					if (mergedBrieven.getScreeningOrganisatie() != null)
					{
						dashboardOrganisaties.add(mergedBrieven.getScreeningOrganisatie());
					}
					String melding = "Door technische reden kon de brief (brieftype: " + brief.getBriefType() + ") niet worden gegenereerd.";

					logService.logGebeurtenis(briefGenerator.getMergeProbleemLogGebeurtenis(), dashboardOrganisaties, client, melding, briefGenerator.getBevolkingsonderzoeken());
					continue;
				}
			}
			if (chunkDocument != null && brievenAanwezig)
			{
				File nieuwePdfMetMergedBrievenVanChunk = File.createTempFile("mergedBrieven", "pdf");
				try (FileOutputStream output = new FileOutputStream(nieuwePdfMetMergedBrievenVanChunk))
				{
					chunkDocument.save(output, asposeService.getPdfSaveOptions());
					chunkDocument.setWarningCallback(warning -> LOG.warn("Warning converting to pdf: " + warning.getDescription()));
				}

				setOrAppendPdf(mergedBrieven, nieuwePdfMetMergedBrievenVanChunk, briefGenerator);

				mergedBrieven = briefGenerator.getMergedBrieven();
				for (Brief brief : succesvolleBrieven)
				{
					brief.setMergedBrieven(mergedBrieven);
					mergedBrieven.setAantalBrieven(mergedBrieven.getAantalBrieven() + 1);
				}
				briefGenerator.verhoogAantalBrievenVanScreeningOrganisatie(mergedBrieven);
			}
			hibernateService.saveOrUpdateAll(chunkItems);
		}
		catch (Exception e)
		{
			briefGenerator.crashMelding("Er is een onbekende fout opgetreden, neem contact op met de helpdesk.", e);
			throw e;
		}
	}

	@Override
	@Transactional
	public <B extends Brief> void setBriefGegenereerd(B brief)
	{
		setBriefGegenereerdInfo(brief, getDefinitiveBriefDefinitie(brief));
	}

	private <B extends Brief> void setBriefGegenereerdInfo(B brief, IDocument documentDefinitie)
	{
		brief.setGegenereerd(true);
		brief.setTemplateNaam(documentDefinitie.getDocument().getNaam());
		if (documentDefinitie instanceof BriefDefinitie)
		{ 
			brief.setBriefDefinitie((BriefDefinitie) documentDefinitie);
		}
		hibernateService.saveOrUpdate(brief);
	}

	private <B extends Brief, MB extends MergedBrieven<?>> Document appendDocument(UploadDocument briefTemplateDocument, B brief, Document chunkDocument,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws Exception
	{
		FileOutputStream output = null;
		try
		{
			File briefTemplate = uploadDocumentService.load(briefTemplateDocument);
			byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);

			Client client = getClientFromBrief(brief);
			MailMergeContext context = getMailMergeContext(brief, client);
			briefGenerator.additionalMergedContext(context);

			Document document;
			BaseDocumentCreator creator = briefGenerator.getDocumentCreator(context);
			if (creator == null)
			{
				document = asposeService.processDocument(briefTemplateBytes, context);
			}
			else
			{
				document = asposeService.processDocumentWithCreator(context, briefTemplate, creator, true);
			}

			if (chunkDocument == null)
			{
				chunkDocument = document;
			}
			else
			{
				chunkDocument.appendDocument(document, ImportFormatMode.USE_DESTINATION_STYLES);
			}

			briefGenerator.additionalActiesWithDocument(context, brief, chunkDocument);
		}
		finally
		{
			if (output != null)
			{
				try
				{
					output.close();
				}
				catch (IOException e)
				{
					briefGenerator.crashMelding("Output stream kon niet worden geclosed!", e);
				}
			}
		}
		return chunkDocument;
	}

	private <B extends Brief, MB extends MergedBrieven<?>> void setOrAppendPdf(MB huidigeMergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws IOException
	{
		if (huidigeMergedBrieven.getMergedBrieven() == null)
		{
			setPdfInMergedBrievenEntiteit(huidigeMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator);
		}
		else
		{
			if (!startNieuwPdfIfNeeded(huidigeMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator))
			{
				joinPdfs(huidigeMergedBrieven, nieuwPdfMetMergedBrieven);
			}
		}
	}

	private <MB extends MergedBrieven<?>, B extends Brief> void setPdfInMergedBrievenEntiteit(MB mergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws IOException
	{
		LOG.info(briefGenerator.getTechnischeLoggingMergedBriefAanmaken(mergedBrieven));
		UploadDocument mergedBrievenPdfContainer = new UploadDocument();
		mergedBrievenPdfContainer.setActief(Boolean.TRUE);
		mergedBrievenPdfContainer.setContentType("application/pdf");
		mergedBrievenPdfContainer.setNaam(briefGenerator.getMergedBrievenNaam(mergedBrieven));
		mergedBrievenPdfContainer.setFile(nieuwPdfMetMergedBrieven);

		uploadDocumentService.saveOrUpdate(mergedBrievenPdfContainer, briefGenerator.getFileStoreLocation(), briefGenerator.getFileStoreId());
		mergedBrieven.setMergedBrieven(mergedBrievenPdfContainer);
		LOG.info("Mergedocument(id = " + mergedBrieven.getId() + ") nieuw aangemaakt op filestore: " + mergedBrievenPdfContainer.getPath());
	}

	private <B extends Brief, MB extends MergedBrieven<?>> boolean startNieuwPdfIfNeeded(MB mergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator)
		throws IOException
	{
		UploadDocument huidigePdfMetMergedBrievenContainer = mergedBrieven.getMergedBrieven();
		File huidigePdfMetMergedBrieven = uploadDocumentService.load(huidigePdfMetMergedBrievenContainer);
		Integer maxMergedBrievenPdfSizeMB = organisatieParameterService.getOrganisatieParameter(mergedBrieven.getScreeningOrganisatie(),
			OrganisatieParameterKey.MAX_MERGED_BRIEVEN_PDF_SIZE_MB);
		if (maxMergedBrievenPdfSizeMB != null && huidigePdfMetMergedBrieven.length() + nieuwPdfMetMergedBrieven.length() > maxMergedBrievenPdfSizeMB * BYTES_TO_MBS)
		{
			MB createdMergedBrieven = briefGenerator.createMergedBrieven(mergedBrieven.getCreatieDatum());
			if (createdMergedBrieven != null)
			{
				completePdf(mergedBrieven);
				correctPdfFileNameIfNeeded(huidigePdfMetMergedBrievenContainer);
				briefGenerator.increasePdfCounter();
				setPdfInMergedBrievenEntiteit(createdMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator);
				return true;
			}
		}
		return false;
	}

	private <MB extends MergedBrieven<?>> void joinPdfs(MB huidigeMergedBrieven, File nieuwPdfMetMergedBrieven) throws IOException
	{
		UploadDocument huidigePdfMetMergeBrievenContainer = huidigeMergedBrieven.getMergedBrieven();
		File huidigePdfMetMergeBrieven = uploadDocumentService.load(huidigePdfMetMergeBrievenContainer);
		File copyHuidigePdfMetMergedBrieven = File.createTempFile("copyMergedBrieven", ".pdf");
		FileUtils.copyFile(huidigePdfMetMergeBrieven, copyHuidigePdfMetMergedBrieven);

		try (FileOutputStream outputStream = new FileOutputStream(huidigePdfMetMergeBrieven))
		{
			PDFMergerUtility pdfMergerUtility = new PDFMergerUtility();
			pdfMergerUtility.addSource(copyHuidigePdfMetMergedBrieven);
			pdfMergerUtility.addSource(nieuwPdfMetMergedBrieven);
			pdfMergerUtility.setDestinationStream(outputStream);
			pdfMergerUtility.mergeDocuments(IOUtils.createMemoryOnlyStreamCache());

			copyHuidigePdfMetMergedBrieven.delete();
			nieuwPdfMetMergedBrieven.delete();
		}
	}

	private void correctPdfFileNameIfNeeded(UploadDocument huidigePdfMetMergedBrievenContainer)
	{
		String huidigePdfNaam = huidigePdfMetMergedBrievenContainer.getNaam();
		huidigePdfNaam = huidigePdfNaam.replace(".pdf", "");
		if (!Pattern.compile("_\\d{2}$").matcher(huidigePdfNaam).find())
		{
			huidigePdfMetMergedBrievenContainer.setNaam(huidigePdfNaam + "_01.pdf");
			hibernateService.saveOrUpdate(huidigePdfMetMergedBrievenContainer);
		}
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief) throws Exception
	{
		Client client = getClientFromBrief(brief);
		File briefTemplate = getBriefDefinitieFile(brief);
		MailMergeContext context = getMailMergeContext(brief, client);
		byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
		Document document = asposeService.processDocument(briefTemplateBytes, context);
		return genereerPdf(document, brief.getBriefType().toString(), true);
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator) throws Exception
	{
		return maakPdfVanBrief(brief, documentCreator, null);
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator,
		Consumer<MailMergeContext> mergeContextConsumer) throws Exception
	{
		Client client = getClientFromBrief(brief);
		File briefTemplate = getBriefDefinitieFile(brief);
		MailMergeContext context = getMailMergeContext(brief, client);
		if (mergeContextConsumer != null)
		{
			mergeContextConsumer.accept(context);
		}
		Document document = asposeService.processDocumentWithCreator(context, briefTemplate, documentCreator, true);
		return genereerPdf(document, brief.getBriefType().toString(), true);
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief, Consumer<MailMergeContext> mergeContextConsumer) throws Exception
	{
		Client client = getClientFromBrief(brief);
		File briefTemplate = getBriefDefinitieFile(brief);
		MailMergeContext context = getMailMergeContext(brief, client);
		if (mergeContextConsumer != null)
		{
			mergeContextConsumer.accept(context);
		}
		byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
		Document document = asposeService.processDocument(briefTemplateBytes, context);
		return genereerPdf(document, brief.getBriefType().toString(), true);
	}

	private <B extends Brief> File getBriefDefinitieFile(B brief)
	{
		IDocument briefDefinitie = getDefinitiveBriefDefinitie(brief);
		UploadDocument uploadDocument = briefDefinitie.getDocument();
		return uploadDocumentService.load(uploadDocument);
	}

	private <B extends Brief> MailMergeContext getMailMergeContext(B brief, Client client)
	{
		MailMergeContext context = new MailMergeContext();
		context.setClient(client);
		context.setBrief(brief);
		return context;
	}

	private <B extends Brief> Client getClientFromBrief(B brief)
	{
		Client client = null;
		if (ClientBrief.class.isAssignableFrom(Hibernate.getClass(brief)))
		{
			ClientBrief<?, ?, ?> clientBrief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(brief);
			client = clientBrief.getClient();
		}
		return client;
	}

	private <B extends Brief> IDocument getDefinitiveBriefDefinitie(B brief)
	{
		IDocument briefDefinitie = getNieuwsteBriefDefinitie(brief.getBriefType());
		Class<B> briefClass = Hibernate.getClass(brief);
		if (ProjectBrief.class.isAssignableFrom(briefClass))
		{
			ProjectBrief projectBrief = (ProjectBrief) HibernateHelper.deproxy(brief);
			briefDefinitie = projectBrief.getDefinitie();
		}
		return briefDefinitie;
	}

	@Override
	public File genereerPdf(Document document, String fileNaam, boolean autoShowPrintdialog) throws Exception
	{
		final File tmpPdfFile = File.createTempFile(fileNaam, ".pdf");

		try (ByteArrayOutputStream tempStream = new ByteArrayOutputStream();
			FileOutputStream pdfOutStream = new FileOutputStream(tmpPdfFile))
		{
			PdfSaveOptions pdfSaveOptions = asposeService.getPdfSaveOptions();
			document.save(tempStream, pdfSaveOptions);
			PDDocument pdfBoxDocument = Loader.loadPDF(new ByteArrayInputStream(tempStream.toByteArray()).readAllBytes());
			if (autoShowPrintdialog)
			{
				PDActionJavaScript javaScript = new PDActionJavaScript(JavaScriptPdfHelper.getPrintJavascript());
				pdfBoxDocument.getDocumentCatalog().setOpenAction(javaScript);
			}
			pdfBoxDocument.save(pdfOutStream);
			pdfBoxDocument.close();
		}
		return tmpPdfFile;
	}

	@Override
	public <B extends Brief> List<B> getNietGegenereerdeBrievenVanBriefTypes(List<B> brieven, List<BriefType> brieftypes)
	{
		return brieven.stream()
			.filter(brief -> brieftypes.contains(brief.getBriefType()) && !BriefUtil.isGegenereerd(brief)).collect(Collectors.toList());
	}

	@Override
	@Transactional
	public void setNietGegenereerdeBrievenOpTegenhouden(ScreeningRonde<?, ?, ?, ?> screeningRonde, Collection<BriefType> brieftypes)
	{
		screeningRonde.getBrieven().stream().filter(brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isNietGegenereerdEnNietVervangen(brief))
			.forEach(brief -> hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true)));
	}

	@Override
	public boolean briefTypeWachtOpKlaarzettenInDezeRonde(ClientBrief<?, ?, ?> brief)
	{
		return briefTypeWachtOpKlaarzettenInDezeRonde(brief.getScreeningRonde(), Collections.singletonList(brief.getBriefType()));
	}

	@Override
	public boolean briefTypeWachtOpKlaarzettenInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes)
	{
		return ronde.getBrieven().stream().anyMatch(
			brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isNietGegenereerdEnNietVervangen(brief));
	}

	@Override
	public boolean briefTypeAlVerstuurdInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isGegenereerd(brief));
	}

	@Override
	public List<ClientBrief<?, ?, ?>> getClientBrieven(Client client)
	{
		return clientBriefRepository.findAll(heeftClient(client));
	}

	@Override
	@Transactional
	public void briefTegenhouden(ClientBrief brief, Account account)
	{
		hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, account, brief.getClient(), BriefUtil.getBriefTypeNaam(brief) + " wordt tegengehouden.",
			BriefUtil.getOnderzoekenUitBriefType(brief));
	}

	@Override
	@Transactional
	public void briefNietMeerTegenhouden(ClientBrief brief, Account account)
	{
		hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, false));
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_DOORVOEREN, account, brief.getClient(), BriefUtil.getBriefTypeNaam(brief) + " was tegengehouden en wordt nu doorgevoerd.",
			BriefUtil.getOnderzoekenUitBriefType(brief));
	}

	@Override
	public boolean briefTypeGemaaktInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> briefTypes)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> briefTypes.contains(brief.getBriefType()));
	}
}
