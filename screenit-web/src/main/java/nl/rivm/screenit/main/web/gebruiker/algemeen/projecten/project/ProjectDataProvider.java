
package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project;

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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.service.ProjectService;
import nl.topicuszorg.util.collections.CollectionUtils;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectDataProvider extends SortableDataProvider<Project, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	private Map<ProjectType, ToegangLevel> toeganglevels;

	private IModel<Project> zoekObject;

	public ProjectDataProvider(IModel<Project> zoekObject, Map<ProjectType, ToegangLevel> toeganglevels)
	{
		Injector.get().inject(this);
		setSort("naam", SortOrder.ASCENDING);
		this.zoekObject = zoekObject;
		this.toeganglevels = toeganglevels;
		setToegangLevelsOpZoekObject();
	}

	@Override
	public Iterator<? extends Project> iterator(long first, long count)
	{
		return projectService
			.getProjecten(ModelUtil.nullSafeGet(zoekObject),
				getInstellingenIds(toeganglevels.get(ProjectType.PROJECT)), getInstellingenIds(toeganglevels.get(ProjectType.BRIEFPROJECT)),
				first, count, new SortState<String>(getSort().getProperty(), getSort().isAscending()))
			.iterator();
	}

	@Override
	public long size()
	{
		return projectService.getCountProjecten(ModelUtil.nullSafeGet(zoekObject), getInstellingenIds(toeganglevels.get(ProjectType.PROJECT)),
			getInstellingenIds(toeganglevels.get(ProjectType.BRIEFPROJECT)));
	}

	private List<Long> getInstellingenIds(ToegangLevel toegangLevel)
	{
		if (toegangLevel != null)
		{
			return organisatieZoekService.getZichtbateInstellingenOpToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie(), toegangLevel,
				Arrays.asList(OrganisatieType.values()));
		}
		return null;
	}

	@Override
	public IModel<Project> model(Project object)
	{
		return ModelUtil.sModel(object);
	}

	private void setToegangLevelsOpZoekObject()
	{
		if (CollectionUtils.isEmpty(zoekObject.getObject().getProjectTypes()))
		{
			List<ProjectType> projectTypesMetRecht = new ArrayList<>();
			for (ProjectType projectType : ProjectType.values())
			{
				if (toeganglevels.get(projectType) != null)
				{
					projectTypesMetRecht.add(projectType);
				}
			}
			zoekObject.getObject().setProjectTypes(projectTypesMetRecht);
		}
	}
}
