
package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid;

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

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

public class HoudbaarheidDataProvider<H extends AbstractHoudbaarheid> extends SortableDataProvider<H, String>
{

	@SpringBean
	private HoudbaarheidService houdbaarheidService;

	private Class<H> clazz;

	public HoudbaarheidDataProvider(Class<H> clazz)
	{
		this.clazz = clazz;
		Injector.get().inject(this);
		setSort("vervalDatum", SortOrder.DESCENDING);
	}

	@Override
	public Iterator<? extends H> iterator(long first, long count)
	{
		return houdbaarheidService.getHoudbaarheidItems(clazz, first, count, toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return houdbaarheidService.countHoudbaarheidItems(clazz);
	}

	@Override
	public IModel<H> model(H object)
	{
		return new SimpleHibernateModel<>(object);
	}
}
