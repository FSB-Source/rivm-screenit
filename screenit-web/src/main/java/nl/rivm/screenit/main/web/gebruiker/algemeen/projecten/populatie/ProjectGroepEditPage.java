package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.populatie;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden.ProjectBestandenOverzicht;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.apache.wicket.validation.validator.RangeValidator;
import org.springframework.format.datetime.DateFormatter;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.TOEVOEGEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_SELECTIE, Recht.GEBRUIKER_BRIEFPROJECT_SELECTIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ProjectGroepEditPage extends ProjectBasePage
{
	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<List<FileUpload>> clientenBestanden = new ListModel<>();

	private final IModel<String> meldingenModel = new Model<>("");

	private final BootstrapDialog dialog;

	private final Date isPushDatumAlGezet;

	public ProjectGroepEditPage(IModel<Project> projectModel)
	{
		this(ModelUtil.ccModel(new ProjectGroep()), projectModel);
	}

	public ProjectGroepEditPage(IModel<ProjectGroep> model, IModel<Project> projectModel)
	{
		super(projectModel);
		Project project = projectModel.getObject();
		ProjectGroep groep = model.getObject();
		if (groep.getProject() == null)
		{
			groep.setProject(project);
		}
		isPushDatumAlGezet = groep.getUitnodigingenPushenNa();
		boolean isNieuweGroep = groep.getId() == null;
		String projectTitel = "Groep aanpassen";
		if (isNieuweGroep)
		{
			projectTitel = "Groep toevoegen";
			groep.setActief(false);
		}

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new Label("projecttitel", Model.of(projectTitel)));

		Form<ProjectGroep> form = new Form<ProjectGroep>("form", model);
		add(form);

		form.add(ComponentHelper.addTextField(form, "naam", true, 24, false));

		Date nu = currentDateSupplier.getDate();
		form.add(ComponentHelper.addTextField(form, "uitnodigenVoorDKvoor", false, 24, Date.class, false).add(DateValidator.minimum(nu))
			.setVisible(ProjectUtil.hasParameterSet(project, ProjectParameterKey.COLON_UITNODIGEN_PRIORITEIT)));

		form.add(new ScreenitDropdown<GroepInvoer>("groepInvoer", GroepInvoer.getGroepinvoerVanSelectieType(project.getGroepSelectieType()), new NaamChoiceRenderer<INaam>())
			.setRequired(true).setEnabled(isNieuweGroep));

		FormComponent<List<FileUpload>> clientenBestand = new FileUploadField("clientenBestand", clientenBestanden)
			.add(new FileValidator(FileType.CSV));
		form.add(clientenBestand);
		clientenBestand.setRequired(isNieuweGroep);
		clientenBestand.setOutputMarkupId(true);

		boolean uitnodigingenPushenInzien = groep.getUitnodigingenPushenNa() != null && nu.after(groep.getUitnodigingenPushenNa()) || nu.after(project.getEindDatum());
		form.add(ComponentHelper.addTextField(form, "uitnodigingenPushenNa", false, 24, Date.class, uitnodigingenPushenInzien)
			.add(RangeValidator.range(currentDateSupplier.getDateMidnight(), project.getEindDatum()))
			.setVisible(!isNieuweGroep && ProjectType.PROJECT.equals(project.getType())));

		IModel<Boolean> skipFouten = new Model<>(Boolean.FALSE);
		form.add(ComponentHelper.newCheckBox("skipFouten", skipFouten).setVisible(isNieuweGroep));

		MultiLineLabel meldingen = new MultiLineLabel("meldingen", meldingenModel);
		meldingen.setEscapeModelStrings(false);
		meldingen.setOutputMarkupId(true);
		form.add(meldingen);

		add(new AjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new PopulatiePage(getProjectModel()));
			}

		});

		add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectGroep groep = form.getModelObject();
				if (getPushDatumVeranderd(groep))
				{
					dialog.openWith(target, new ConfirmPanel(IDialog.CONTENT_ID, Model.of(getString("confirm.uitnodigingen.pushen")), null, new DefaultConfirmCallback()
					{

						private static final long serialVersionUID = 1L;

						@Override
						public void onYesClick(AjaxRequestTarget target)
						{
							opslaan(target, form);
						}

					}, dialog));
				}
				else
				{
					opslaan(target, form);
				}
			}

		});
		add(new ProjectPaspoortPanel("projectPasspoort", projectModel));
	}

	private void opslaan(AjaxRequestTarget target, Form<ProjectGroep> form)
	{
		ProjectGroep groep = form.getModelObject();
		Project project = groep.getProject();
		if (groep.getUitnodigenVoorDKvoor() != null && project.getEindDatum().before(groep.getUitnodigenVoorDKvoor()))
		{
			error("'Uitnodigen voor DK v\u00F3\u00F3r' mag niet na de 'Einddatum' van het project liggen.");
		}
		if (groep.getUitnodigingenPushenNa() != null && project.getEindDatum().before(groep.getUitnodigingenPushenNa()))
		{
			error("De push datum mag niet na de einddatum van het project liggen.");
		}
		if (!hasErrorMessage())
		{
			if (groep.getId() == null && GroepInvoer.IMPORT.equals(groep.getGroepInvoer()))
			{
				if (clientenBestanden.getObject().size() == 1)
				{
					try
					{
						if (project.getEindeInstroom() != null && groep.getUitnodigenVoorDKvoor() != null
							&& groep.getUitnodigenVoorDKvoor().after(project.getEindeInstroom()))
						{
							ScreenitSession.get().warn(String.format(getString("einde.groep.na.einde.instroom"), groep.getNaam()));
						}
						FileUpload clientenBestand = clientenBestanden.getObject().get(0);
						projectService.queueProjectBestandVoorPopulatie(groep, clientenBestand.getContentType(), clientenBestand.getClientFileName(),
							clientenBestand.writeToTempFile(), ScreenitSession.get().getLoggedInAccount());

						setResponsePage(new ProjectBestandenOverzicht(ProjectGroepEditPage.this.getProjectModel()));
					}
					catch (Exception e)
					{
						error("Bestand kon niet worden geimporteerd.");
					}
				}
				else
				{
					error("Geen bestand opgegeven om te kunnen importeren.");
				}
			}
			else if (groep.getId() != null)
			{
				if (getPushDatumVeranderd(groep))
				{
					DateFormatter formatter = new DateFormatter("dd-MM-yyyy");
					String melding = "Project: " + project.getNaam() + ", Groep: " + groep.getNaam() + ", Pushdatum: "
						+ formatter.print(groep.getUitnodigingenPushenNa(), Constants.LOCALE_NL);
					logService.logGebeurtenis(LogGebeurtenis.PROJECT_GROEP_GEPUSHT, ScreenitSession.get().getLoggedInAccount(), melding);
				}
				hibernateService.saveOrUpdate(groep);
				if (project.getEindeInstroom() != null && groep.getUitnodigenVoorDKvoor() != null && groep.getUitnodigenVoorDKvoor().after(project.getEindeInstroom()))
				{
					ScreenitSession.get().warn("Groep is opgeslagen." + String.format(getString("einde.groep.na.einde.instroom"), groep.getNaam()));
				}
				else
				{
					ScreenitSession.get().info("Groep is opgeslagen.");
				}
				setResponsePage(new PopulatiePage(getProjectModel()));
			}
		}
	}

	private boolean getPushDatumVeranderd(ProjectGroep groep)
	{
		return isPushDatumAlGezet == null && groep.getUitnodigingenPushenNa() != null
			|| isPushDatumAlGezet != null && groep.getUitnodigingenPushenNa() != null && !isPushDatumAlGezet.equals(groep.getUitnodigingenPushenNa());
	}

}
