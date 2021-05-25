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
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectBestandVerwerkingDataProvider extends SortableDataProvider<ProjectBestandVerwerkingEntry, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ProjectDao projectDao;

	private IModel<ProjectBestandVerwerkingEntry> entryModel;

	public ProjectBestandVerwerkingDataProvider(IModel<ProjectBestandVerwerkingEntry> entryModel)
	{
		Injector.get().inject(this);
		setSort("regelNummer", SortOrder.ASCENDING);
		this.entryModel = entryModel;
	}

	@Override
	public Iterator<? extends ProjectBestandVerwerkingEntry> iterator(long first, long count)
	{
		return projectDao.getProjectBestandVerwerkingEntries(ModelUtil.nullSafeGet(entryModel), first, count, new SortState<>(getSort().getProperty(), getSort().isAscending()));

	}

	@Override
	public long size()
	{
		return projectDao.getAantalProjectbestandVerwerkingEntries(ModelUtil.nullSafeGet(entryModel));
	}

	@Override
	public IModel<ProjectBestandVerwerkingEntry> model(ProjectBestandVerwerkingEntry object)
	{
		return ModelUtil.sModel(object);
	}

}
