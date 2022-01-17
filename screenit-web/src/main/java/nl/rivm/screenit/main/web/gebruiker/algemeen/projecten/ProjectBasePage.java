package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.attributen.ProjectAttributenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden.ProjectBestandenOverzicht;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven.ProjectBriefActiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.inactiveren.ProjectClientenWijzigenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.populatie.PopulatiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project.ProjectOverzicht;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project.ProjectStatusPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.uitslag.ProjectUitslagUploadenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectBasePage extends AlgemeenPage
{
	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<Project> projectModel;

	public ProjectBasePage(IModel<Project> model)
	{
		projectModel = model;
		if (isDefaultModelProjectModel())
		{
			setDefaultModel(model);
		}
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.projecten.overzicht", ProjectOverzicht.class));
		contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.gegevens", ProjectStatusPage.class)
		{
			@Override
			protected ProjectBasePage createPage()
			{
				return new ProjectStatusPage(getProjectModel());
			}
		});
		if (getProjectModel() != null)
		{
			if (ProjectType.PROJECT.equals(getProjectType()) && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_PROJECT_SELECTIE, Actie.INZIEN) ||
				ProjectType.BRIEFPROJECT.equals(getProjectType())
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BRIEFPROJECT_SELECTIE, Actie.INZIEN))
			{
				contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.populatie", PopulatiePage.class)
				{
					@Override
					protected ProjectBasePage createPage()
					{
						return new PopulatiePage(getProjectModel());
					}
				});
			}
			if (ProjectType.PROJECT.equals(getProjectType()) && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_PROJECT_BRIEVEN, Actie.INZIEN) ||
				ProjectType.BRIEFPROJECT.equals(getProjectType())
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BRIEFPROJECT_BRIEVEN, Actie.INZIEN))
			{
				contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.briefactie", ProjectBriefActiePage.class)
				{
					@Override
					protected ProjectBasePage createPage()
					{
						return new ProjectBriefActiePage(getProjectModel());
					}
				});
			}
		}
		contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.clientenwijzigen", ProjectClientenWijzigenPage.class)
		{
			@Override
			protected ProjectBasePage createPage()
			{
				return new ProjectClientenWijzigenPage(getProjectModel());
			}
		});
		contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.attributen", ProjectAttributenPage.class)
		{
			@Override
			protected ProjectBasePage createPage()
			{
				return new ProjectAttributenPage(getProjectModel());
			}
		});

		if (getProjectType().equals(ProjectType.PROJECT) && !ProjectUtil.getStatus(projectModel.getObject(), currentDateSupplier.getDate()).equals(ProjectStatus.BEEINDIGD))
		{
			contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.uitslagen", ProjectUitslagUploadenPage.class)
			{
				@Override
				protected ProjectBasePage createPage()
				{
					return new ProjectUitslagUploadenPage(getProjectModel());
				}
			});
		}
		contextMenuItems.add(new ProjectGebruikerMenuItem("menu.algemeen.project.bestanden", ProjectBestandenOverzicht.class)
		{
			@Override
			protected ProjectBasePage createPage()
			{
				return new ProjectBestandenOverzicht(getProjectModel());
			}
		});
		return contextMenuItems;
	}

	protected Recht getRechtOverzicht()
	{
		return ProjectType.BRIEFPROJECT.equals(getProjectType()) ? Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT : Recht.GEBRUIKER_PROJECT_OVERZICHT;
	}

	protected Recht getRechtSelectie()
	{
		return ProjectType.BRIEFPROJECT.equals(getProjectType()) ? Recht.GEBRUIKER_BRIEFPROJECT_SELECTIE : Recht.GEBRUIKER_PROJECT_SELECTIE;
	}

	private ProjectType getProjectType()
	{
		return getProjectModel().getObject().getType();
	}

	protected ToegangLevel getToegangsLevel(Recht recht, Actie actie)
	{
		return autorisatieService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), actie, true, recht);
	}

	protected IModel<Project> getProjectModel()
	{
		return projectModel;
	}

	private abstract class ProjectGebruikerMenuItem extends GebruikerMenuItem
	{
		@SuppressWarnings("unchecked")
		public ProjectGebruikerMenuItem(String resourceTag, Class<? extends ProjectBasePage> targetPageClass)
		{
			super(resourceTag, targetPageClass);
		}

		@Override
		public IndicatingAjaxLink<?> createWicketLink(String markupId)
		{
			return new IndicatingAjaxLink<>(markupId, getProjectModel())
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					setResponsePage(createPage());
				}
			};
		}

		protected abstract ProjectBasePage createPage();
	}

	protected boolean isDefaultModelProjectModel()
	{
		return true;
	}

	@Override
	protected boolean getPageLarge()
	{
		return false;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(projectModel);
	}
}
