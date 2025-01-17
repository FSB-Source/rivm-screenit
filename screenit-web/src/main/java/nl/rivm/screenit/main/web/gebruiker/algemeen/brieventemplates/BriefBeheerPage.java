package nl.rivm.screenit.main.web.gebruiker.algemeen.brieventemplates;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.component.validator.FormuliernummerValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.service.BaseBriefService;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.AjaxEditableLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Bytes;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = false,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_BEHEER_BRIEVEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class BriefBeheerPage extends AlgemeenPage
{
	@SpringBean
	private BaseBriefService briefService;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private HibernateService hibernateService;

	private final BootstrapDialog dialog;

	private final IModel<BvoZoekCriteria> zoekCriteria;

	private IModel<List<BriefDefinitie>> briefDefinities = null;

	private final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_BRIEVEN, Actie.AANPASSEN);

	public BriefBeheerPage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		zoekCriteria = Model.of(new BvoZoekCriteria());
		zoekCriteria.getObject().setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		final WebMarkupContainer brievenContainer = new WebMarkupContainer("brievenContainer");
		brievenContainer.setOutputMarkupId(true);
		add(brievenContainer);
		final var uploadHeaderContainer = new WebMarkupContainer("uploadHeader");
		uploadHeaderContainer.setVisible(magAanpassen);
		add(uploadHeaderContainer);
		add(new FilterBvoFormPanel<>("bvoFilter", zoekCriteria, true, true)
		{

			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				briefDefinities = null;
				target.add(brievenContainer);
			}
		});

		brievenContainer.add(new PropertyListView<>("brieven", (IModel<List<BriefDefinitie>>) this::getBriefDefinities)
		{
			private final IModel<List<FileUpload>> files = new ListModel<>();

			@Override
			protected void populateItem(final ListItem<BriefDefinitie> item)
			{
				var briefDefinitie = item.getModelObject();
				if (briefDefinitie.getGeldigTot() != null)
				{
					item.add(new AttributeModifier("class", "display-none subrow " + subrowClass(briefDefinitie)));
				}

				var briefType = briefDefinitie.getBriefType();

				if (briefDefinitie.getGeldigTot() == null)
				{
					item.add(new Label("bvo", Bevolkingsonderzoek.getAfkortingen(briefType.getOnderzoeken())));
					item.add(new Label("type", briefType.getWeergaveNaam()).setVisible(briefDefinitie.getGeldigTot() == null));
				}
				else
				{
					item.add(new Label("bvo", ""));
					item.add(new Label("type", ""));
				}
				item.add(new Label("documentNaam", new PropertyModel<>(item.getModel(), "document.naam")));
				item.add(new WebMarkupContainer("geenDocument")
				{
					@Override
					protected void onConfigure()
					{
						super.onConfigure();
						setVisible(item.getModelObject().getDocument() == null);
					}
				});
				var uitklapknop = new WebMarkupContainer("uitklapknop").setVisible(briefDefinitie.getVolgnummer() != 1 && briefDefinitie.getGeldigTot() == null);
				uitklapknop.add(new AttributeAppender("class", " briefTemplate "));
				uitklapknop.add(new AttributeAppender("data-value", subrowClass(briefDefinitie)));
				item.add(uitklapknop);

				item.add(new Label("volgnummer", Model.of(briefDefinitie.getVolgnummer())));

				AjaxEditableLabel<String> formulerierNummer = new AjaxEditableLabel<String>("formulierNummer")
				{

					@Override
					protected String defaultNullLabel()
					{
						return "&gtKlik hier voor invoer&lt";
					}

					@Override
					protected void onSubmit(AjaxRequestTarget target)
					{
						super.onSubmit(target);
						Object modelObject = getDefaultModelObject();
						if (modelObject instanceof String)
						{
							item.getModelObject().setFormulierNummer((String) modelObject);
							hibernateService.saveOrUpdate(item.getModelObject());
						}
					}

					@Override
					protected FormComponent<String> newEditor(MarkupContainer parent, String componentId, IModel<String> model)
					{
						FormComponent<String> newEditor = super.newEditor(parent, componentId, model);
						newEditor.add(new FormuliernummerValidator());
						return newEditor;
					}

				};
				item.add(formulerierNummer);
				formulerierNummer.setRequired(true).setVisible(briefType.getVerzendendeOrganisatieType().equals(OrganisatieType.INPAKCENTRUM));

				item.add(DateLabel.forDatePattern("geldigVanaf", Model.of(briefDefinitie.getLaatstGewijzigd()), "dd-MM-yyyy HH:mm:ss"));
				item.add(DateLabel.forDatePattern("geldigTot", Model.of(briefDefinitie.getGeldigTot()), "dd-MM-yyyy HH:mm:ss"));
				item.add(new Label("geuploadDoor", new PropertyModel<>(item.getModel(), "uploader.medewerker.naamVolledig")));
				item.add(new UploadDocumentLink("download", new PropertyModel<>(item.getModel(), "document"), true)
					.setVisible(item.getModelObject().getDocument() != null));

				Form<Void> uploadForm = new Form<>("uploadForm");
				uploadForm.setFileMaxSize(Bytes.kilobytes(700));
				uploadForm.add(new FileUploadField("fileUpload", files).add(new FileValidator(FileType.WORD_NIEUW)).setRequired(true));
				uploadForm.add(new AjaxSubmitLink("uploaden")
				{
					@Override
					public void onSubmit(AjaxRequestTarget target)
					{
						if (files.getObject().size() == 1)
						{

							final var fileUpload = files.getObject().get(0);

							try
							{
								List<Project> projecten = projectService.getAllProjectenWhereProjectBriefActieHasBriefType(item.getModelObject().getBriefType());
								var uploadDocument = ScreenitSession.get().fileUploadToUploadDocument(fileUpload);
								if (!projecten.isEmpty())
								{
									dialog.openWith(target, new ConfirmPanel(IDialog.CONTENT_ID, new StringResourceModel("confirm.title", this),
										new StringResourceModel("confirm.content", this).setParameters(namenVanDeProjecten(projecten)),
										new DefaultConfirmCallback()
										{
											@Override
											public void onYesClick(AjaxRequestTarget target)
											{
												saveNieuweBriefDefinitie(item.getModelObject(), uploadDocument, target, brievenContainer);
											}
										}, dialog));
								}
								else
								{
									saveNieuweBriefDefinitie(item.getModelObject(), uploadDocument, target, brievenContainer);
								}
							}
							catch (Exception e)
							{
								LOG.error("Fout bij opslaan van template", e);
								BriefBeheerPage.this.error(getString("error.onbekend"));
							}
						}
						else
						{
							BriefBeheerPage.this.error(getString("error.onjuistaantalfiles"));
						}
					}
				});
				final var uploadColumnContainer = new WebMarkupContainer("uploadColumn");
				uploadColumnContainer.setVisible(magAanpassen);
				uploadColumnContainer.add(uploadForm);
				item.add(uploadColumnContainer);

				if (briefDefinitie.getGeldigTot() != null || !magAanpassen)
				{
					uploadForm.setVisible(false);
				}
			}

		});
	}

	private String subrowClass(BriefDefinitie briefDefinitie)
	{
		return "subrow_" + briefDefinitie.getBriefType().toString();
	}

	private List<BriefDefinitie> getBriefDefinities()
	{
		if (briefDefinities == null)
		{
			briefDefinities = ModelUtil.listModel(briefService.getBriefDefinities(zoekCriteria.getObject(), new PropertyComparator<>("codeEnNaam", true, true)));
		}
		return briefDefinities.getObject();
	}

	private void saveNieuweBriefDefinitie(BriefDefinitie definitie, UploadDocument uploadDocument, AjaxRequestTarget target, WebMarkupContainer brievenContainer)
	{
		try
		{
			var nieuweBriefDefinitie = new BriefDefinitie();
			nieuweBriefDefinitie.setBriefType(definitie.getBriefType());
			nieuweBriefDefinitie.setFormulierNummer(definitie.getFormulierNummer());
			nieuweBriefDefinitie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
			briefService.saveBriefDefinitie(nieuweBriefDefinitie, uploadDocument.getFile(), uploadDocument.getContentType(), uploadDocument.getNaam());
		}
		catch (Exception e)
		{
			LOG.error("Fout bij opslaan van template", e);
			error(getString("error.onbekend"));
			return;
		}
		info(getString("bestand.geuploaded"));
		briefDefinities = null;
		target.add(brievenContainer);
	}

	private String namenVanDeProjecten(List<Project> projecten)
	{
		return projecten.stream().map(Project::getNaam).collect(Collectors.joining(", "));
	}

}
