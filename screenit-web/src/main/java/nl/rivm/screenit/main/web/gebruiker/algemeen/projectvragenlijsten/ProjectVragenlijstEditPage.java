package nl.rivm.screenit.main.web.gebruiker.algemeen.projectvragenlijsten;

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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.VragenlijstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadLinkPanel;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.PreviewCellPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.DocSaveOptions;
import com.aspose.words.Document;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_DEFINITIE_VRAGENLIJSTEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class ProjectVragenlijstEditPage extends ProjectVragenlijstenBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ProjectVragenlijstEditPage.class);

	private final TransparentWebMarkupContainer fragments;

	@SpringBean
	private VragenlijstService vragenlijstService;

	@SpringBean
	private FileService fileService;

	@SpringBean
	private AsposeService asposeService;

	private IModel<List<FileUpload>> files = new ListModel<>();

	private IModel<ProjectVragenlijst> projectVragenlijstModel;

	private Component table;

	public ProjectVragenlijstEditPage(IModel<ProjectVragenlijst> model)
	{
		this.projectVragenlijstModel = model;
		add(new EditForm("form", model));
		WebMarkupContainer container = new WebMarkupContainer("definitiesContainer");
		container.setOutputMarkupId(true);

		table = getTable(model);
		container.add(table);
		add(container);
		fragments = new TransparentWebMarkupContainer("fragments");
		add(fragments);
	}

	private Component getTable(final IModel<ProjectVragenlijst> vragenlijst)
	{
		List<IColumn<ScreenitFormulierInstantie, String>> columns = new ArrayList<IColumn<ScreenitFormulierInstantie, String>>();
		columns.add(new PropertyColumn<ScreenitFormulierInstantie, String>(Model.of("Versie"), "formulierDefinitieNaam"));
		columns.add(new DateTimePropertyColumn<ScreenitFormulierInstantie, String>(Model.of("Uploaddatum"), "creatieDatum"));
		columns.add(new PropertyColumn<ScreenitFormulierInstantie, String>(Model.of("Uploader"), "uploader.medewerker.naamVolledig"));

		columns.add(new AbstractColumn<ScreenitFormulierInstantie, String>(Model.of("Genereer template"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ScreenitFormulierInstantie>> cellItem, String componentId, IModel<ScreenitFormulierInstantie> rowModel)
			{
				cellItem.add(new DownloadFragment(componentId, rowModel));
			}

		});

		columns.add(new AbstractColumn<ScreenitFormulierInstantie, String>(Model.of("Upload definitief template"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ScreenitFormulierInstantie>> cellItem, String componentId, final IModel<ScreenitFormulierInstantie> rowModel)
			{
				cellItem.add(new UploadFragment(componentId, rowModel));
			}

		});
		columns.add(new AbstractColumn<ScreenitFormulierInstantie, String>(Model.of("Download definitief template"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ScreenitFormulierInstantie>> cellItem, String componentId, IModel<ScreenitFormulierInstantie> rowModel)
			{
				if (rowModel.getObject().getTemplateVanGebruiker() != null)
				{
					cellItem.add(new DownloadDefinitiefFragment(componentId, rowModel));
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}
		});
		columns.add(new AbstractColumn<ScreenitFormulierInstantie, String>(Model.of("Web preview"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ScreenitFormulierInstantie>> cellItem, String componentId, IModel<ScreenitFormulierInstantie> rowModel)
			{
				cellItem.add(new PreviewCellPanel(componentId, rowModel)
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
					{
						return ProjectVragenlijstEditPage.class;
					}

					@Override
					protected void terug()
					{
						setResponsePage(new ProjectVragenlijstEditPage(vragenlijst));

					}

				});
			}

		});

		Component dataTable;
		if (vragenlijst.getObject().getId() != null)
		{
			dataTable = new ScreenitDataTable<ScreenitFormulierInstantie, String>("definities", columns,
				new FormulierDefinitiesProvider(ModelUtil.cModel(new ScreenitFormulierInstantie()), "formulierDefinitieNaam", vragenlijst), 10, new Model<String>("definities"))
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected boolean isRowClickable(IModel<ScreenitFormulierInstantie> rowModel)
				{
					return false;
				}

				@Override
				public void onClick(AjaxRequestTarget target, IModel<ScreenitFormulierInstantie> model)
				{
				}
			};
		}
		else
		{
			dataTable = new EmptyPanel("definities");
		}
		return dataTable;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.vragenlijsten", ProjectVragenlijstenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.vragenlijsten.vragenlijst.edit", false, ProjectVragenlijstEditPage.class));

		return contextMenuItems;
	}

	private class EditForm extends ScreenitForm<ProjectVragenlijst>
	{

		private static final long serialVersionUID = 1L;

		private final IModel<List<FileUpload>> fileUploads = new ListModel<>();

		public EditForm(String id, IModel<ProjectVragenlijst> model)
		{
			super(id, model);

			boolean inzien = !ScreenitSession.get().checkPermission(Recht.GEBRUIKER_DEFINITIE_VRAGENLIJSTEN, Actie.AANPASSEN);
			ComponentHelper.addTextField(this, "naam", true, 255, inzien);
			add(new FileUploadField("definitieBestand", fileUploads)
				.add(new FileValidator(FileType.EXCEL_NIEUW))
				.setVisible(!vragenlijstService.isVragenlijstGekoppeldAanHolder(model.getObject().getId(), ProjectVragenlijstAntwoordenHolder.class)));

			add(new IndicatingAjaxLink<Void>("annuleren")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					setResponsePage(ProjectVragenlijstenPage.class);
				}
			});

			add(new IndicatingAjaxButton("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					File definitieFile = null;
					List<FileUpload> filesUploaded = fileUploads.getObject();
					if (CollectionUtils.isNotEmpty(filesUploaded))
					{
						try
						{
							definitieFile = filesUploaded.get(0).writeToTempFile();
						}
						catch (Exception e)
						{

							LOG.debug("Kon file niet wegschrijven naar temp", e.getMessage());
							ScreenitSession.get().error("Fout bij opslaan van bestand (" + e.getMessage() + ")");
						}
					}
					ProjectVragenlijst vragenlijst = (ProjectVragenlijst) getForm().getModelObject();
					if (vragenlijst.getId() == null && definitieFile == null)
					{
						ScreenitSession.get()
							.error("In een nieuwe vragenlijst moet een formulier bestand geuploaded worden. " + "Selecteer eerst een xls(x) bestand voor opslaan.");
					}
					else
					{
						boolean isNieuw = vragenlijst.getId() == null;
						try
						{
							vragenlijstService.updateVragenlijst(vragenlijst, definitieFile, ScreenitSession.get().getLoggedInInstellingGebruiker());
							ScreenitSession.get().success("Vragenlijst is opgeslagen.");
							setResponsePage(ProjectVragenlijstenPage.class);
						}
						catch (Exception e)
						{
							if (isNieuw && vragenlijst.getId() != null)
							{
								ProjectVragenlijst newVragenlijst = new ProjectVragenlijst();
								newVragenlijst.setNaam(vragenlijst.getNaam());
								getForm().setDefaultModelObject(newVragenlijst);
							}
							LOG.error("Fout bij opslaan van vragenlijst formulier", e);
							ScreenitSession.get().error("Fout bij opslaan van vragenlijst formulier (" + e.getMessage() + ").");
						}
					}
				}
			});

		}
	}

	private class UploadFragment extends Fragment
	{
		private static final long serialVersionUID = 1L;

		public UploadFragment(String id, final IModel<ScreenitFormulierInstantie> model)
		{
			super(id, "fragmentUpload", fragments, new CompoundPropertyModel<>(model));
			setOutputMarkupId(true);

			Form<Void> uploadForm = new Form<>("uploadForm");

			uploadForm.add(new FileUploadField("fileUpload", files)
				.add(new FileValidator(FileType.WORD_NIEUW))
				.setRequired(true));
			uploadForm.add(new AjaxSubmitLink("uploaden")
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void onSubmit(AjaxRequestTarget target)
				{
					List<FileUpload> uploadedFiles = files.getObject();
					if (uploadedFiles.size() == 1)
					{

						FileUpload fileUpload = uploadedFiles.get(0);
						ScreenitFormulierInstantie formulierInstantie = model.getObject();
						formulierInstantie.setTemplateUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
						try
						{
							vragenlijstService.saveOrUpdateTemplate(formulierInstantie, fileUpload.getContentType(), fileUpload.getClientFileName(), fileUpload.writeToTempFile());
							target.add(table);
							ScreenitSession.get().info(ProjectVragenlijstEditPage.this.getString("bestand.geuploaded"));
						}
						catch (Exception e)
						{
							LOG.error("Fout bij opslaan van template in definitie", e);
							ScreenitSession.get().info(ProjectVragenlijstEditPage.this.getString("error.onbekend"));
						}

						if (uploadedFiles != null)
						{
							uploadedFiles.clear();
						}
					}
					else
					{
						ScreenitSession.get().error(ProjectVragenlijstEditPage.this.getString("error.onjuistaantalfiles"));
					}
				}
			});
			add(uploadForm);
		}
	}

	private class DownloadFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public DownloadFragment(String id, final IModel<ScreenitFormulierInstantie> model)
		{
			super(id, "fragmentDownload", fragments, new CompoundPropertyModel<>(model));
			setOutputMarkupId(true);

			add(new ResourceLink<>("download", new AbstractResource()
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected ResourceResponse newResourceResponse(Attributes attributes)
				{
					ResourceResponse resourceResponse = new ResourceResponse();
					resourceResponse.setFileName("VragenlijstDefaultTemplate.doc");
					resourceResponse.setContentType("application/vnd.openxmlformats-officedocument.wordprocessingml.document");
					resourceResponse.setCacheDuration(Duration.NONE);
					resourceResponse.setWriteCallback(new WriteCallback()
					{
						@Override
						public void writeData(Attributes attributes) throws IOException
						{
							try (OutputStream outputStream = attributes.getResponse().getOutputStream();)
							{
								MailMergeContext mailMergeContext = new MailMergeContext();
								mailMergeContext.setVragenlijstNaam(projectVragenlijstModel.getObject().getNaam());

								Document document = asposeService.processVragenlijst(mailMergeContext, model.getObject(), false);
								DocSaveOptions docSaveOptions = new DocSaveOptions();
								document.save(outputStream, docSaveOptions);
							}
							catch (Exception e)
							{
								LOG.error("Fout bij het genereren van het template.", e);
								error("Er is een probleem opgetreden met het genereren van de PDF.");
							}
						}
					});
					return resourceResponse;
				}
			}));
		}
	}

	private class DownloadDefinitiefFragment extends Fragment
	{
		private static final long serialVersionUID = 1L;

		public DownloadDefinitiefFragment(String id, final IModel<ScreenitFormulierInstantie> model)
		{
			super(id, "downloadDefinitiefFragment", fragments, new CompoundPropertyModel<>(model));
			setOutputMarkupId(true);

			add(new UploadDocumentDownloadLinkPanel("downloadHuidigTemplate", ModelUtil.sModel(model.getObject().getTemplateVanGebruiker())));
			add(new Label("templateUploader.medewerker.naamVolledig"));
		}
	}
}
