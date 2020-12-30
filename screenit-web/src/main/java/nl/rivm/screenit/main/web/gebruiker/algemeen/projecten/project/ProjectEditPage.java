package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.GroepSelectieType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.TOEVOEGEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_OVERZICHT, Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ProjectEditPage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ProjectEditPage.class);

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private LogService logService;

	private ScreenitDropdown<Instelling> organisatieDropdown;

	private WebMarkupContainer medewerkersContainer;

	private List<Bevolkingsonderzoek> gekozenBevolkingsonderzoeken = new ArrayList<>();

	private EditProjectParametersPanel parametersPanel;

	public ProjectEditPage(IModel<Project> model)
	{
		model = ModelUtil.cModel(model.getObject());
		setDefaultModel(model);
		Project project = model.getObject();

		if (project.getBevolkingsonderzoeken() != null)
		{
			gekozenBevolkingsonderzoeken.addAll(project.getBevolkingsonderzoeken());
		}

		Boolean magSelectiePopulatieGegevensAanpassen = project.getGroepen() == null || project.getGroepen().size() == 0;

		String title = String.format(getString("toevoegen"), project.getType());
		if (project.getId() != null)
		{
			title = String.format(getString("bewerken"), project.getType());
		}
		add(new Label("title", Model.of(title)));

		Form<Project> form = new Form<Project>("form", model);
		add(form);

		ComponentHelper.addTextField(form, "naam", true, 255, false).add(new UniqueFieldValidator(Project.class, project.getId(), "naam", hibernateService));

		ScreenitListMultipleChoice<Instelling> soDropDown = new ScreenitListMultipleChoice<Instelling>("screeningOrganisaties",
			new SimpleListHibernateModel<>(organisatieZoekService.getAllActieveOrganisatiesWithType(ScreeningOrganisatie.class)), new ChoiceRenderer<Instelling>("naam"));
		soDropDown.setRequired(true);
		form.add(soDropDown);

		organisatieDropdown = new ScreenitDropdown<Instelling>("organisatie", new SimpleListHibernateModel<>(hibernateService.loadAll(Instelling.class)),
			new ChoiceRenderer<Instelling>("naam"));
		organisatieDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				Instelling instelling = organisatieDropdown.getConvertedInput();
				WebMarkupContainer container = getMedewerkersContainer(instelling);
				medewerkersContainer.replaceWith(container);
				medewerkersContainer = container;
				target.add(medewerkersContainer);
			}
		});
		organisatieDropdown.setRequired(true);
		form.add(organisatieDropdown);

		medewerkersContainer = getMedewerkersContainer(project.getOrganisatie());
		form.add(medewerkersContainer);

		Date vandaag = currentDateSupplier.getDateMidnight();

		Date startDatum = form.getModelObject().getStartDatum();
		boolean enabled = startDatum == null || currentDateSupplier.getDate().before(startDatum);
		DatePicker<Date> startDatumDatePicker = ComponentHelper.newDatePicker("startDatum", new PropertyModel<Date>(form.getModel(), "startDatum"), enabled);
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.add(DateValidator.minimum(vandaag));
		form.add(startDatumDatePicker);

		Date eindDatum = form.getModelObject().getEindDatum();
		enabled = eindDatum == null || currentDateSupplier.getDate().before(eindDatum);
		DatePicker<Date> correspondentieDatum = ComponentHelper.newDatePicker("eindDatum", new PropertyModel<Date>(form.getModel(), "eindDatum"), enabled);
		correspondentieDatum.add(DateValidator.minimum(vandaag));
		correspondentieDatum.setRequired(true);
		form.add(correspondentieDatum);
		form.add(new DependantDateValidator(startDatumDatePicker, correspondentieDatum, DependantDateValidator.Operator.AFTER));

		Date eindeInstroom = form.getModelObject().getEindeInstroom();
		enabled = eindeInstroom == null || currentDateSupplier.getDate().before(eindeInstroom);
		DatePicker<Date> eindeInstroomDatePicker = ComponentHelper.newDatePicker("correspondentieDatum", new PropertyModel<Date>(form.getModel(), "eindeInstroom"), enabled);
		eindeInstroomDatePicker.add(DateValidator.minimum(vandaag));
		form.add(eindeInstroomDatePicker);
		form.add(new DependantDateValidator(startDatumDatePicker, eindeInstroomDatePicker, DependantDateValidator.Operator.AFTER));

		ScreenitListMultipleChoice<Bevolkingsonderzoek> bvoDropdown = new ScreenitListMultipleChoice<Bevolkingsonderzoek>("bevolkingsonderzoeken",
			new ListModel<>(Arrays.asList(Bevolkingsonderzoek.values())), new EnumChoiceRenderer<>());
		bvoDropdown.setRequired(true);
		bvoDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				gekozenBevolkingsonderzoeken.clear();
				gekozenBevolkingsonderzoeken.addAll(bvoDropdown.getConvertedInput());

				Project innerProject = (Project) getDefaultModelObject();
				innerProject.getParameters().removeIf(parameter -> !gekozenBevolkingsonderzoeken.contains(parameter.getKey().getBevolkingsonderzoek()));

				List<ProjectParameterKey> gefilterdeParameters = Arrays.stream(ProjectParameterKey.values())
					.filter(p -> gekozenBevolkingsonderzoeken.contains(p.getBevolkingsonderzoek()) && p.getProjectType().equals(innerProject.getType()))
					.collect(Collectors.toList());

				for (ProjectParameterKey parameterKey : gefilterdeParameters)
				{
					if (innerProject.getParameters().stream().noneMatch(p -> p.getKey().equals(parameterKey)))
					{
						ProjectParameter parameter = new ProjectParameter();
						parameter.setProject(innerProject);
						parameter.setKey(parameterKey);
						innerProject.getParameters().add(parameter);
					}
				}
				maakProjectParameterPanelAan();
				target.add(parametersPanel);
			}
		});
		form.add(bvoDropdown);

		RadioChoice<Boolean> booleanRadio = new BooleanRadioChoice("anoniem", new PropertyModel<Boolean>(form.getModel(), "anoniem"));
		booleanRadio.setPrefix("<label class=\"radio\">");
		booleanRadio.setSuffix("</label>");
		booleanRadio.setRequired(true);
		form.add(booleanRadio);

		RadioChoice<GroepSelectieType> groepSelectieType = new RadioChoice<GroepSelectieType>("groepSelectieType", Arrays.asList(GroepSelectieType.values()),
			new EnumChoiceRenderer<>(this));
		groepSelectieType.setPrefix("<label class=\"radio\">");
		groepSelectieType.setSuffix("</label>");
		groepSelectieType.setRequired(true);
		groepSelectieType.setEnabled(magSelectiePopulatieGegevensAanpassen);
		form.add(groepSelectieType);

		RadioChoice<Boolean> excludeerBezwaarRadio = new BooleanRadioChoice("excludeerBezwaar", new PropertyModel<>(form.getModel(), "excludeerBezwaar"));
		excludeerBezwaarRadio.setPrefix("<label class=\"radio\">");
		excludeerBezwaarRadio.setSuffix("</label>");
		excludeerBezwaarRadio.setOutputMarkupId(true);
		excludeerBezwaarRadio.setVisible(project.getType().equals(ProjectType.PROJECT));
		form.add(excludeerBezwaarRadio);

		List<Bevolkingsonderzoek> onderzoekKeuzes = Arrays.asList(Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA);
		ScreenitListMultipleChoice<Bevolkingsonderzoek> excludeerOpenRondeDropDown = new ScreenitListMultipleChoice<Bevolkingsonderzoek>("excludeerOpenRonde",
			new ListModel<>(onderzoekKeuzes), new EnumChoiceRenderer<>());
		excludeerOpenRondeDropDown.setVisible(project.getType().equals(ProjectType.PROJECT));
		form.add(excludeerOpenRondeDropDown);

		ScreenitListMultipleChoice<Bevolkingsonderzoek> excludeerAfmeldingDropDown = new ScreenitListMultipleChoice<Bevolkingsonderzoek>("excludeerAfmelding",
			new ListModel<>(onderzoekKeuzes), new EnumChoiceRenderer<>());
		excludeerAfmeldingDropDown.setEnabled(magSelectiePopulatieGegevensAanpassen);
		excludeerAfmeldingDropDown.setVisible(project.getType().equals(ProjectType.PROJECT));
		form.add(excludeerAfmeldingDropDown);

		maakProjectParameterPanelAan();
		form.add(parametersPanel);

		TextArea<String> area = ComponentHelper.newTextArea("opmerkingen", 255);
		form.add(area);

		add(new IndicatingAjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectOverzicht());
			}
		});

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
				Project project = (Project) form.getModelObject();
				Date startDatum = project.getStartDatum();
				Date eindDatum = project.getEindDatum();
				Date eindeInstroom = project.getEindeInstroom();
				if (eindeInstroom == null || eindeInstroom.after(startDatum) && eindeInstroom.before(eindDatum))
				{
					boolean heeftWarningsOfErrors = false;
					if (project.getGroepen() != null)
					{
						for (ProjectGroep groep : project.getGroepen())
						{
							if (!ProjectUtil.hasParameterSet(project, ProjectParameterKey.COLON_UITNODIGEN_PRIORITEIT))
							{
								if (groep.getUitnodigenVoorDKvoor() != null)
								{
									ScreenitSession.get().warn(String.format(getString("einde.groep.verwijderd"), groep.getNaam()));
									groep.setUitnodigenVoorDKvoor(null);
									heeftWarningsOfErrors = true;
								}
							}
							else if (project.getEindeInstroom() != null && groep.getUitnodigenVoorDKvoor() != null
								&& groep.getUitnodigenVoorDKvoor().after(project.getEindeInstroom()))
							{
								ScreenitSession.get().warn(String.format(getString("einde.groep.na.einde.instroom"), groep.getNaam()));
								heeftWarningsOfErrors = true;
							}
						}
					}

					if (!heeftWarningsOfErrors)
					{
						projectService.saveOrUpdateProject(project, ScreenitSession.get().getLoggedInInstellingGebruiker());
						ScreenitSession.get().info(getString("project.succesvol.opgeslagen"));
						setResponsePage(new ProjectStatusPage(ModelUtil.sModel(project)));
					}
				}
				else
				{
					if (currentDateSupplier.getDate().after(startDatum))
					{
						error("De einddatum instroom moet tussen de vandaag (" + format.format(currentDateSupplier.getDate()) + ") en einddatum (" + format.format(eindDatum)
							+ ") liggen.");
					}
					else
					{
						error("De einddatum instroom moet tussen de startdatum (" + format.format(startDatum) + ") en einddatum (" + format.format(eindDatum) + ") liggen.");
					}
				}
			}
		});

	}

	private void maakProjectParameterPanelAan()
	{
		IModel<Project> model = (IModel<Project>) getDefaultModel();
		Project project = model.getObject();
		EditProjectParametersPanel nieuwParametersPanel = new EditProjectParametersPanel("parameters", model, gekozenBevolkingsonderzoeken);
		nieuwParametersPanel.setOutputMarkupId(true);
		nieuwParametersPanel.setVisible(project.getType().equals(ProjectType.PROJECT));

		if (parametersPanel != null)
		{
			parametersPanel.replaceWith(nieuwParametersPanel);
		}
		parametersPanel = nieuwParametersPanel;
	}

	private WebMarkupContainer getMedewerkersContainer(Instelling instelling)
	{
		WebMarkupContainer medewerkersContainer = new WebMarkupContainer("medewerkersContainer");
		medewerkersContainer.setOutputMarkupId(true);

		List<InstellingGebruiker> lijstMetMogelijkeMedewerkers = new ArrayList<InstellingGebruiker>();
		if (instelling != null && instelling.getOrganisatieMedewerkers() != null && !instelling.getOrganisatieMedewerkers().isEmpty())
		{
			lijstMetMogelijkeMedewerkers = instelling.getOrganisatieMedewerkers();
		}

		ScreenitDropdown<InstellingGebruiker> contactPersoonDropDown = new ScreenitDropdown<InstellingGebruiker>("contactpersoon",
			new SimpleListHibernateModel<>(lijstMetMogelijkeMedewerkers), new IChoiceRenderer<InstellingGebruiker>()
			{
				@Override
				public Object getDisplayValue(InstellingGebruiker object)
				{
					return object.getMedewerker().getNaamVolledig();
				}

				@Override
				public String getIdValue(InstellingGebruiker object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public InstellingGebruiker getObject(String id, IModel<? extends List<? extends InstellingGebruiker>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(i -> i.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}

			});
		contactPersoonDropDown.setRequired(true);
		medewerkersContainer.add(contactPersoonDropDown);
		ScreenitListMultipleChoice<InstellingGebruiker> medewerkersMulti = new ScreenitListMultipleChoice<InstellingGebruiker>("medewerkers",
			new SimpleListHibernateModel<>(lijstMetMogelijkeMedewerkers), new IChoiceRenderer<InstellingGebruiker>()
			{
				@Override
				public Object getDisplayValue(InstellingGebruiker object)
				{
					return object.getMedewerker().getNaamVolledig();
				}

				@Override
				public String getIdValue(InstellingGebruiker object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public InstellingGebruiker getObject(String id, IModel<? extends List<? extends InstellingGebruiker>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(i -> i.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}

			});
		medewerkersContainer.add(medewerkersMulti);

		return medewerkersContainer;
	}

	@Override
	protected boolean getPageLarge()
	{
		return false;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.projecten.overzicht", ProjectOverzicht.class));
		return contextMenuItems;
	}

}
