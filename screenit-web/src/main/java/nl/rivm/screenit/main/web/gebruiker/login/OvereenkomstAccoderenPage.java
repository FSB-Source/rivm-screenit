package nl.rivm.screenit.main.web.gebruiker.login;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.OutputStream;
import java.util.List;

import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.Page;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;
import com.aspose.words.PdfSaveOptions;

public class OvereenkomstAccoderenPage extends LoginBasePage
{

	private static final Logger LOG = LoggerFactory.getLogger(OvereenkomstAccoderenPage.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private OvereenkomstService overeenkomstService;

	@SpringBean
	private FileService fileService;

	@SpringBean
	private AsposeService asposeService;

	public OvereenkomstAccoderenPage(IModel<InstellingGebruiker> instellingGebruiker)
	{
		List<AbstractAfgeslotenOvereenkomst> accoLijst = overeenkomstService.getTeAccoderenOvereenkomsten(instellingGebruiker.getObject());
		if (CollectionUtils.isNotEmpty(accoLijst) && OvereenkomstType.KWALITEITSOVEREENKOMST == accoLijst.get(0).getOvereenkomst().getOvereenkomst())
		{
			add(new Label("accoderenTekst", getString("label.accoderentekst.kwaliteitsovereenkomst")));
		}
		else
		{
			add(new Label("accoderenTekst", getString("label.accoderentekst.organisatie")));
		}

		add(new ListView<AbstractAfgeslotenOvereenkomst>("overeenkomsten", new SimpleListHibernateModel<>(accoLijst))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<AbstractAfgeslotenOvereenkomst> item)
			{
				item.add(new Label("code", item.getModelObject().getCode()));
				if (item.getModelObject().getGescandDocument() != null)
				{
					item.add(new UploadDocumentLink("download", new PropertyModel<UploadDocument>(item.getModel(), "gescandDocument"), false));
				}
				else
				{
					item.add(new ResourceLink<>("download", new AbstractResource()
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected ResourceResponse newResourceResponse(Attributes attributes)
						{
							ResourceResponse response = new ResourceResponse();
							response.setFileName(item.getModelObject().getCode().replace(" ", "_") + ".pdf");
							response.setContentType("application/pdf");
							response.getHeaders().addHeader("Cache-Control", "no-cache");
							response.setContentDisposition(ContentDisposition.ATTACHMENT);

							response.setWriteCallback(new WriteCallback()
							{

								@Override
								public void writeData(Attributes attributes)
								{
									try (OutputStream outputStream = attributes.getResponse().getOutputStream();)
									{
										File file = fileService.load(item.getModelObject().getOvereenkomst().getDocument());
										MailMergeContext mailMergeContext = new MailMergeContext();
										mailMergeContext.setOvereenkomst(item.getModelObject());
										Document document = asposeService.processDocument(FileUtils.readFileToByteArray(file), mailMergeContext);

										document.save(outputStream, new PdfSaveOptions());
									}
									catch (IOException e)
									{
										LOG.error("Fout bij laden uploaddocument: " + e.getMessage(), e);
									}
									catch (Exception e)
									{
										LOG.error("Error bij mailmerge: " + e.getMessage(), e);
									}
								}
							});
							return response;
						}
					}));
				}
			}
		});
		add(new Link<Void>("naarInlog")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				Class<? extends Page> homePage = Application.get().getHomePage();
				ScreenitSession.get().logout();
				setResponsePage(homePage);
			}
		});
		add(new Link<InstellingGebruiker>("akkoord", instellingGebruiker)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				InstellingGebruiker gebruiker = getModelObject();
				overeenkomstService.accodeerOvereenkomsten(gebruiker, ScreenitSession.get().getLoggedInAccount());
				Component pageForInstellingGebruiker = ScreenitSession.get().getPageForInstellingGebruiker(gebruiker);
				if (pageForInstellingGebruiker != null)
				{
					setResponsePage((WebPage) pageForInstellingGebruiker);
				}
				else
				{
					error(getString("error.nietvoldoende.rechten"));
				}
			}
		});
	}
}
