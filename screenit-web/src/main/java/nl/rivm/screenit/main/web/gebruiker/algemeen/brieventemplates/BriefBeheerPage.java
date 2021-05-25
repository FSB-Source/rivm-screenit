package nl.rivm.screenit.main.web.gebruiker.algemeen.brieventemplates;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.comparator.EnumResourceComparator;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.BriefTypeLabel;
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
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectService;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
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
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Bytes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = false,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_BEHEER_BRIEVEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class BriefBeheerPage extends AlgemeenPage
{

	private static final Logger LOG = LoggerFactory.getLogger(BriefBeheerPage.class);

	@SpringBean
	private BaseBriefService briefService;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	private final BootstrapDialog dialog;

	private final IModel<BvoZoekCriteria> zoekCriteria;

	private IModel<List<BriefDefinitie>> briefDefinities = null;

	public BriefBeheerPage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		zoekCriteria = Model.of(new BvoZoekCriteria());
		zoekCriteria.getObject().setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		final WebMarkupContainer brievenContainer = new WebMarkupContainer("brievenContainer");
		brievenContainer.setOutputMarkupId(true);
		add(brievenContainer);
		add(new FilterBvoFormPanel<BvoZoekCriteria>("bvoFilter", zoekCriteria, true, true)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				briefDefinities = null;
				target.add(brievenContainer);
			}
		});

		brievenContainer.add(new PropertyListView<BriefDefinitie>("brieven", new IModel<List<BriefDefinitie>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<BriefDefinitie> getObject()
			{
				return getBriefDefinities();
			}

		})
		{

			private static final long serialVersionUID = 1L;

			private final IModel<List<FileUpload>> files = new ListModel<>();

			@Override
			protected void populateItem(final ListItem<BriefDefinitie> item)
			{
				BriefDefinitie briefDefinitie = item.getModelObject();

				if (briefDefinitie.getGeldigTot() != null)
				{
					item.add(new AttributeModifier("class", "subrow " + subrowClass(briefDefinitie)));
					item.add(new AttributeModifier("hidden", "true"));
				}

				BriefType briefType = briefDefinitie.getBriefType();
				if (briefDefinitie.getGeldigTot() == null)
				{
					item.add(new Label("bvo", Bevolkingsonderzoek.getAfkortingen(briefType.getOnderzoeken())));
					item.add(new BriefTypeLabel("type", briefType).setVisible(briefDefinitie.getGeldigTot() == null));
				}
				else
				{
					item.add(new Label("bvo", ""));
					item.add(new Label("type", ""));
				}
				item.add(new Label("documentNaam", new PropertyModel<>(item.getModel(), "document.naam")));
				item.add(new WebMarkupContainer("geenDocument")
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void onConfigure()
					{
						super.onConfigure();
						setVisible(item.getModelObject().getDocument() == null);
					}
				});
				Component uitklapknop = new WebMarkupContainer("uitklapknop").setVisible(briefDefinitie.getVolgnummer() != 1 && briefDefinitie.getGeldigTot() == null);
				uitklapknop.add(new AttributeAppender("onclick", "if (this.subrowsVisible) { $('." + subrowClass(briefDefinitie)
					+ "').hide(); this.src = '../../assets/images/icons/arrow.gif'; this.subrowsVisible = false; }" +
					"else { $('." + subrowClass(briefDefinitie) + "').show(); this.src = '../../assets/images/icons/arrowDown.gif'; this.subrowsVisible = true; }", ";"));
				item.add(uitklapknop);
				item.add(new Label("volgnummer", Model.of(briefDefinitie.getVolgnummer())));

				AjaxEditableLabel<String> formulerierNummer = new AjaxEditableLabel<String>("formulierNummer")
				{

					private static final long serialVersionUID = 1L;

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
				item.add(new UploadDocumentLink("download", new PropertyModel<UploadDocument>(item.getModel(), "document"), true)
					.setVisible(item.getModelObject().getDocument() != null));

				Form<Void> uploadForm = new Form<>("uploadForm");
				uploadForm.setFileMaxSize(Bytes.kilobytes(700));
				uploadForm.add(new FileUploadField("fileUpload", files)
					.add(new FileValidator(FileType.WORD_NIEUW))
					.setRequired(true));
				uploadForm.add(new AjaxSubmitLink("uploaden")
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onSubmit(AjaxRequestTarget target)
					{
						if (files.getObject().size() == 1)
						{

							final FileUpload fileUpload = files.getObject().get(0);

							List<Project> projecten = projectService.getAllProjectenWhereProjectBriefActieHasBriefType(item.getModelObject().getBriefType());
							if (projecten.size() > 0)
							{

								dialog.openWith(target,
									new ConfirmPanel(IDialog.CONTENT_ID, Model.of("Weet u zeker dat u de brief template wilt aanpassen?"),
										Model
											.of("De volgende actieve en toekomstige projecten hebben aangepaste brieven op deze brief template: " + namenVanDeProjecten(projecten)),
										new DefaultConfirmCallback()
										{
											private static final long serialVersionUID = 1L;

											@Override
											public void onYesClick(AjaxRequestTarget target)
											{
												saveBriefDefinitie(item.getModelObject(), fileUpload, target, brievenContainer);
											}

										}, dialog));
							}
							else
							{
								saveBriefDefinitie(item.getModelObject(), fileUpload, target, brievenContainer);
							}
						}
						else
						{
							BriefBeheerPage.this.error(getString("error.onjuistaantalfiles"));
						}
					}
				});
				item.add(uploadForm);
				if (briefDefinitie.getGeldigTot() != null)
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
			briefDefinities = ModelUtil.listRModel(briefService.getBriefDefinities(zoekCriteria.getObject(), new EnumResourceComparator<BriefType>(this)));
		}
		return briefDefinities.getObject();
	}

	private void saveBriefDefinitie(BriefDefinitie definitie, FileUpload fileUpload, AjaxRequestTarget target, WebMarkupContainer brievenContainer)
	{
		try
		{
			definitie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
			briefService.saveBriefDefinitie(definitie, fileUpload.writeToTempFile(), fileUpload.getContentType(), fileUpload.getClientFileName());

			logAction(LogGebeurtenis.BRIEF_TOEGEVOEGD, definitie);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij opslaan van template", e);
			error(getString("error.onbekend"));
			return;
		}
		info(getString("bestand.geuploaded"));
		target.add(brievenContainer);
	}

	private String namenVanDeProjecten(List<Project> projecten)
	{
		boolean first = true;
		String projectNamen = "";
		for (Project project : projecten)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				projectNamen += ", ";
			}
			projectNamen += project.getNaam();
		}
		return projectNamen;
	}

	private void logAction(LogGebeurtenis gebeurtenis, BriefDefinitie brief)
	{
		logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(),
			"Brief geupload: " + brief.getDocument().getNaam() + " Type: " + getString("BriefType." + brief.getBriefType().name()), brief.getBriefType().getOnderzoeken());
	}
}
