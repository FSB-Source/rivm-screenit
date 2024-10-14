package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.attributen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.model.project.ProjectAttribuut_.NAAM;

public class ProjectAttributenDataProvider extends SortableDataProvider<ProjectAttribuut, String>
{
	@SpringBean
	private ProjectService projectService;

	private final IModel<ProjectAttribuut> filterModel;

	public ProjectAttributenDataProvider(IModel<ProjectAttribuut> filterModel)
	{
		Injector.get().inject(this);
		setSort(NAAM, SortOrder.ASCENDING);
		this.filterModel = filterModel;
	}

	@Override
	public Iterator<? extends ProjectAttribuut> iterator(long first, long count)
	{
		return projectService.getProjectAttributen(ModelUtil.nullSafeGet(filterModel), first, count, WicketSpringDataUtil.toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return projectService.getAantalProjectAttributen(ModelUtil.nullSafeGet(filterModel));
	}

	@Override
	public IModel<ProjectAttribuut> model(ProjectAttribuut object)
	{
		return ModelUtil.cModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(filterModel);
	}

}
