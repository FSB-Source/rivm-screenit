package nl.rivm.screenit.main.web.gebruiker.clienten.project;

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

import java.util.Iterator;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.Project_;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientProjectenDataProvider extends SortableDataProvider<ProjectClient, String>
{

	@SpringBean
	private ProjectService projectService;

	private final IModel<ProjectClient> zoekObject;

	public ClientProjectenDataProvider(IModel<ProjectClient> zoekObject)
	{
		Injector.get().inject(this);
		setSort(ProjectGroep_.PROJECT + "." + Project_.NAAM, SortOrder.ASCENDING);
		this.zoekObject = zoekObject;
	}

	@Override
	public Iterator<? extends ProjectClient> iterator(long first, long count)
	{
		return projectService.getClientProjecten(ModelUtil.nullSafeGet(zoekObject), first, count, WicketSpringDataUtil.toSpringSort(getSort()))
			.iterator();
	}

	@Override
	public long size()
	{
		return projectService.getCountClientProjecten(ModelUtil.nullSafeGet(zoekObject));
	}

	@Override
	public IModel<ProjectClient> model(ProjectClient object)
	{
		return ModelUtil.ccModel(object);
	}

}
