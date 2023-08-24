package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

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

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.NavigeerNaarCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentColumn;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.TOEVOEGEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_BRIEVEN, Recht.GEBRUIKER_BRIEFPROJECT_BRIEVEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA })
public class ProjectBriefActiePage extends ProjectBasePage
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private WebMarkupContainer actieContainer;

	private IModel<ProjectBriefActie> briefActieModel;

	private BootstrapDialog dialog;

	public ProjectBriefActiePage(IModel<Project> model)
	{
		super(model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		briefActieModel = ModelUtil.ccModel(new ProjectBriefActie());
		briefActieModel.getObject().setProject(model.getObject());
		briefActieModel.getObject().setActief(Boolean.TRUE);

		if (!ScreenitSession.get().isZoekObjectGezetForComponent(ProjectBriefActiePage.class))
		{
			ScreenitSession.get().setZoekObject(ProjectBriefActiePage.class, briefActieModel);
		}

		add(new AjaxLink<>("briefToevoegen", model)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new BriefActieEditPage(getModel()));
			}

			@Override
			public void onConfigure()
			{
				super.onConfigure();
				Project project = getModelObject();
				Date eindDatum = project.getEindDatum();
				Date nu = currentDateSupplier.getDate();
				setVisible(!eindDatum.before(nu));
			}

		});

		actieContainer = getBriefActieContainer();
		add(actieContainer);
		add(new ProjectPaspoortPanel("projectPasspoort", model));
	}

	private WebMarkupContainer getBriefActieContainer()
	{
		WebMarkupContainer briefActieContainer = new WebMarkupContainer("briefActieContainer");
		briefActieContainer.setOutputMarkupId(true);
		List<IColumn<ProjectBriefActie, String>> columns = new ArrayList<IColumn<ProjectBriefActie, String>>();
		columns.add(new EnumPropertyColumn<ProjectBriefActie, String, ProjectBriefActieType>(Model.of("Soort"), "type", "type"));
		columns.add(new AbstractColumn<>(Model.of("Moment"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<ProjectBriefActie>> cellItem, String componentId, IModel<ProjectBriefActie> rowModel)
			{
				cellItem.add(new Label(componentId, getMomentText(rowModel.getObject())));
			}
		});

		columns.add(new PropertyColumn<>(Model.of("Naam"), "document.naam", "document.naam"));

		FileValidator validator = new FileValidator(FileType.WORD_NIEUW);
		columns.add(new UploadDocumentColumn<>(Model.of("Uploaden"), "document", briefActieContainer, validator)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBriefActie>> item, String componentId, IModel<ProjectBriefActie> rowModel)
			{
				super.populateItem(item, componentId, rowModel);
				if (rowModel.getObject() != null && Boolean.FALSE.equals(rowModel.getObject().getActief()))
				{
					item.setEnabled(false);
				}
			}

			@Override
			protected void saveNewUploadDocument(AjaxRequestTarget target, ProjectBriefActie object, UploadDocument document)
			{
				try
				{
					uploadDocumentService.saveOrUpdate(document, FileStoreLocation.PROJECT_BRIEF_TEMPLATES, object.getProject().getId());
					object.setDocument(document);
					object.setLaatstGewijzigd(currentDateSupplier.getDate());
					hibernateService.saveOrUpdate(object);
					info("bestand succesvol geupload");

					Project project = object.getProject();
					String melding = getString(EnumStringUtil.getPropertyString(project.getType())) + ": " + project.getNaam() +
						" Briefsoort: " + getString(EnumStringUtil.getPropertyString(object.getType()));

					if (object.getBriefType() != null)
					{
						melding += getString(EnumStringUtil.getPropertyString(object.getBriefType()));
					}

					if (ProjectType.BRIEFPROJECT.equals(object.getProject().getType()))
					{
						logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_BRIEF_ACTIE_GEWIJZIGD, ScreenitSession.get().getLoggedInAccount(), melding);
					}
					else
					{
						logService.logGebeurtenis(LogGebeurtenis.PROJECT_BRIEF_ACTIE_GEWIJZIGD, ScreenitSession.get().getLoggedInAccount(), melding);
					}
				}
				catch (IOException e)
				{
					error("Bestand kon niet worden geupload");
				}

			}
		});

		columns.add(new UploadDocumentDownloadColumn<>(Model.of("Downloaden"), "document"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Laatst gewijzigd"), "laatstGewijzigd", "laatstGewijzigd", new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new PropertyColumn<>(Model.of("Vragenlijst"), "vragenlijst.naam", "vragenlijst.naam"));

		columns.add(new AbstractColumn<>(Model.of("Details / Test Projectbrief template"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBriefActie>> cellItem, String componentId, IModel<ProjectBriefActie> rowModel)
			{
				ProjectBriefActie actie = rowModel.getObject();
				if (actie.getDocument() != null)
				{
					cellItem.add(new NavigeerNaarCellPanel<>(componentId, rowModel)
					{
						@Override
						protected boolean magNavigerenNaar(IModel<ProjectBriefActie> rowModel)
						{
							return true;
						}

						@Override
						protected void onNavigeerNaar(AjaxRequestTarget target, IModel<ProjectBriefActie> rowModel)
						{
							setResponsePage(new ProjectBriefActieTemplatePage(ModelUtil.sModel((Project) ProjectBriefActiePage.this.getDefaultModelObject()),
								ModelUtil.sModel(rowModel.getObject())));
						}
					});
				}
				else
				{
					cellItem.add(new Label(componentId, ""));
				}
			}
		});

		columns.add(new ActiefPropertyColumn<>(Model.of("Verwijderen"), "actief", briefActieContainer, briefActieModel, true, dialog,
			"projectBriefActiePage.verwijderen")
		{

			@Override
			protected void onAfterToggleActief(AjaxRequestTarget target, ProjectBriefActie actiefObject)
			{
				hibernateService.saveOrUpdate(actiefObject);

				WebMarkupContainer container = getBriefActieContainer();
				ProjectBriefActiePage.this.actieContainer.replaceWith(container);
				ProjectBriefActiePage.this.actieContainer = container;
				target.add(ProjectBriefActiePage.this.actieContainer);

				Project project = actiefObject.getProject();
				String typebrief = getString(EnumStringUtil.getPropertyString(actiefObject.getType()));

				ProjectBriefActie projectBriefActie = briefActieModel.getObject();
				if (projectBriefActie.getProject().getType().equals(ProjectType.BRIEFPROJECT))
				{
					String melding = typebrief + " geinactiveerd van Briefproject: " + project.getNaam();
					logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_BRIEF_ACTIE_GEINACTIVEERD, ScreenitSession.get().getLoggedInAccount(), melding);
				}
				else
				{
					String melding = typebrief + " geinactiveerd van Project: " + project.getNaam();
					logService.logGebeurtenis(LogGebeurtenis.PROJECT_BRIEF_ACTIE_GEINACTIVEERD, ScreenitSession.get().getLoggedInAccount(), melding);
				}

				info("Projectbrief succesvol geinactiveerd!");
			}

			@Override
			protected boolean mayToggle(ProjectBriefActie actiefObject)
			{
				return actiefObject.getActief();
			}
		});

		ScreenitDataTable<ProjectBriefActie, String> dataTable = new ScreenitDataTable<ProjectBriefActie, String>("projectBriefacties", columns,
			new BriefActieDataProvider(briefActieModel), 10, Model.of("briefacties"))
		{
			@Override
			protected boolean isRowClickable(IModel<ProjectBriefActie> rowModel)
			{
				return false;
			}
		};
		briefActieContainer.add(dataTable);
		return briefActieContainer;
	}

	private String getMomentText(ProjectBriefActie projectBriefActie)
	{
		var momentTekst = "";
		var type = projectBriefActie.getType();

		switch (type)
		{
		case DATUM:
		case VANAF_DATUM:
			momentTekst = DateUtil.formatShortDate(projectBriefActie.getDatum());
			break;
		case XDAGENNAY:
			momentTekst = projectBriefActie.getAantalDagen() + " dagen na ";
			momentTekst += getBriefNaam(projectBriefActie);
			break;
		case XMETY:
			momentTekst = "Tegelijk met ";
			momentTekst += getBriefNaam(projectBriefActie);
			break;
		case VERVANGENDEBRIEF:
			momentTekst += getBriefNaam(projectBriefActie);
			break;
		default:
		}
		return momentTekst;
	}

	private String getBriefNaam(ProjectBriefActie briefActie)
	{
		var naam = "";
		var briefType = briefActie.getBriefType();
		if (briefType != null)
		{
			naam = getString(EnumStringUtil.getPropertyString(briefType, null));
		}
		return naam;
	}

	@Override
	protected boolean getPageLarge()
	{
		return true;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(briefActieModel);
	}
}
