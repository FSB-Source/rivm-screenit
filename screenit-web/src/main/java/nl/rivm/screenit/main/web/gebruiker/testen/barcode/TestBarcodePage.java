package nl.rivm.screenit.main.web.gebruiker.testen.barcode;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.IOException;
import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.SubmitLink;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.handler.resource.ResourceStreamRequestHandler;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.encoding.UrlEncoder;
import org.apache.wicket.util.resource.FileResourceStream;
import org.apache.wicket.util.resource.IResourceStream;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

@Slf4j
@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.TESTEN, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.CERVIX })
public class TestBarcodePage extends TestenBasePage
{

	@SpringBean
	private CervixTestService cervixTestService;

	@SpringBean
	private TestService testService;

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private BaseBriefService briefService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private UploadDocumentService uploadDocService;

	private IModel<String> barcodes;

	public TestBarcodePage()
	{
		barcodes = new CompoundPropertyModel<>("");
		Form<Void> form = new Form<>("form");
		form.add(new TextField<>("barcodes", barcodes));
		form.add(new SubmitLink("printen")
		{
			@Override
			public void onSubmit()
			{
				if (StringUtils.isNotBlank(barcodes.getObject()))
				{
					Document alleDocumenten = null;
					String[] barcodesArray = StringUtils.split(barcodes.getObject(), ',');
					for (String barcode : barcodesArray)
					{
						GbaPersoon gbaPersoon = new GbaPersoon();
						gbaPersoon.setBsn(TestBsnGenerator.getValideBsn());
						gbaPersoon.setGeslacht(Geslacht.VROUW);
						gbaPersoon.setAchternaam("Doe-" + gbaPersoon.getBsn());
						gbaPersoon.setVoornaam("Jane");
						gbaPersoon.setVoorletters("J");
						gbaPersoon.setGeboortedatum(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusYears(50)));
						gbaPersoon.setGbaAdres(new BagAdres());

						CervixScreeningRonde ronde = cervixTestService.geefScreeningRonde(gbaPersoon);

						Document document = cervixTestService.geefBarcodeUitnodigingsIdTestPdf(ronde.getLaatsteUitnodiging());
						if (alleDocumenten == null)
						{
							alleDocumenten = document;
						}
						else
						{
							try
							{
								alleDocumenten.appendDocument(document, ImportFormatMode.KEEP_SOURCE_FORMATTING);
							}
							catch (Exception e)
							{
								LOG.error(e.getMessage());
							}
						}
					}

					File tmpPdfFile = null;
					try
					{
						tmpPdfFile = briefService.genereerPdf(alleDocumenten, "test_template_sticker", false);
					}
					catch (Exception e)
					{
						LOG.error("Fout bij maken PDF", e);
					}
					if (tmpPdfFile == null)
					{
						throw new IllegalStateException(getClass().getName() + " bestand was null, kapotstuk.");
					}
					String fileName = "barcode.pdf";
					fileName = UrlEncoder.QUERY_INSTANCE.encode(fileName, getRequest().getCharset());
					IResourceStream resourceStream = new FileResourceStream(new org.apache.wicket.util.file.File(tmpPdfFile));
					getRequestCycle().scheduleRequestHandlerAfterCurrent(new ResourceStreamRequestHandler(resourceStream).setFileName(fileName)
						.setContentDisposition(ContentDisposition.ATTACHMENT).setCacheDuration(Duration.ZERO));
				}
			}
		});
		form.add(new SubmitLink("printenTemplate")
		{
			@Override
			public void onSubmit()
			{
				UploadDocument briefTemplateDoc = briefService.getNieuwsteBriefDefinitie(BriefType.CERVIX_UITNODIGING).getDocument();
				File briefTemplate = null;
				if (briefTemplateDoc != null)
				{
					briefTemplate = uploadDocService.load(briefTemplateDoc);
				}

				if (StringUtils.isNotBlank(barcodes.getObject()) && briefTemplate != null)
				{
					Document alleDocumenten = null;
					String[] barcodesArray = StringUtils.split(barcodes.getObject(), ',');
					for (String barcode : barcodesArray)
					{
						GbaPersoon gbaPersoon = new GbaPersoon();
						gbaPersoon.setBsn(TestBsnGenerator.getValideBsn());
						gbaPersoon.setGeslacht(Geslacht.VROUW);
						gbaPersoon.setAchternaam("Doe-" + gbaPersoon.getBsn());
						gbaPersoon.setVoornaam("Jane");
						gbaPersoon.setVoorletters("J");
						gbaPersoon.setGeboortedatum(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusYears(50)));
						gbaPersoon.setGbaAdres(new BagAdres());

						CervixScreeningRonde ronde = cervixTestService.geefScreeningRonde(gbaPersoon);

						MailMergeContext context = new MailMergeContext();
						context.setCervixUitnodiging(ronde.getLaatsteUitnodiging());
						context.setClient(ronde.getDossier().getClient());

						Document document = null;
						try
						{
							byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
							document = asposeService.processDocument(briefTemplateBytes, context);
						}
						catch (IOException e)
						{
							LOG.error(e.getMessage());
						}
						catch (Exception e)
						{
							LOG.error(e.getMessage());
							error("Er is iets misgegaan met het genereren van de pdf.");
						}

						if (alleDocumenten == null)
						{
							alleDocumenten = document;
						}
						else
						{
							try
							{
								alleDocumenten.appendDocument(document, ImportFormatMode.KEEP_SOURCE_FORMATTING);
							}
							catch (Exception e)
							{
								LOG.error(e.getMessage());
							}
						}
					}

					File tmpPdfFile = null;
					try
					{
						tmpPdfFile = briefService.genereerPdf(alleDocumenten, "test_template_sticker", false);
					}
					catch (Exception e)
					{
						LOG.error("Fout bij maken barcode pdf", e);
					}
					if (tmpPdfFile == null)
					{
						throw new IllegalStateException(getClass().getName() + " bestand was null, kapotstuk.");
					}
					String fileName = "template.pdf";
					fileName = UrlEncoder.QUERY_INSTANCE.encode(fileName, getRequest().getCharset());
					IResourceStream resourceStream = new FileResourceStream(new org.apache.wicket.util.file.File(tmpPdfFile));
					getRequestCycle().scheduleRequestHandlerAfterCurrent(new ResourceStreamRequestHandler(resourceStream).setFileName(fileName)
						.setContentDisposition(ContentDisposition.ATTACHMENT).setCacheDuration(Duration.ZERO));
				}
			}
		});

		add(form);
	}

	public IModel<String> getBarcodes()
	{
		return barcodes;
	}

	public void setBarcodes(IModel<String> barcodes)
	{
		this.barcodes = barcodes;
	}

}
