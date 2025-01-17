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
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.model.project.ProjectAttribuut_.NAAM;
import static nl.rivm.screenit.model.project.ProjectClientAttribuut_.ATTRIBUUT;

public class ClientProjectAttributenDataProvider extends SortableDataProvider<ProjectClientAttribuut, String>
{
	private final IModel<ProjectClient> filterModel;

	@SpringBean
	private ProjectService projectService;

	public ClientProjectAttributenDataProvider(IModel<ProjectClient> filterModel)
	{
		Injector.get().inject(this);
		this.filterModel = filterModel;
		setSort(ATTRIBUUT + "." + NAAM, SortOrder.ASCENDING);
	}

	@Override
	public Iterator<? extends ProjectClientAttribuut> iterator(long first, long count)
	{
		return projectService.getAttributenVoorProjectClient(ModelUtil.nullSafeGet(filterModel), first, count, WicketSpringDataUtil.toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return projectService.getAantalAttributenVoorProjectClient(ModelUtil.nullSafeGet(filterModel));
	}

	@Override
	public IModel<ProjectClientAttribuut> model(ProjectClientAttribuut object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(filterModel);
	}
}
