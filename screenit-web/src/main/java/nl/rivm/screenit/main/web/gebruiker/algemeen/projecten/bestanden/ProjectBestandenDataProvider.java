package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden;

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

import java.util.Iterator;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectBestandenDataProvider extends SortableDataProvider<ProjectBestand, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ProjectDao projectDao;

	private IModel<ProjectBestand> filterModel;

	public ProjectBestandenDataProvider(IModel<ProjectBestand> filterModel)
	{
		Injector.get().inject(this);
		setSort("uploadDatum", SortOrder.DESCENDING);
		this.filterModel = filterModel;
	}

	@Override
	public Iterator<? extends ProjectBestand> iterator(long first, long count)
	{
		return projectDao.getProjectBestanden(ModelUtil.nullSafeGet(filterModel), first, count, new SortState<>(getSort().getProperty(), getSort().isAscending()));
	}

	@Override
	public long size()
	{
		return projectDao.getAantalProjectBestanden(ModelUtil.nullSafeGet(filterModel));
	}

	@Override
	public IModel<ProjectBestand> model(ProjectBestand object)
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
