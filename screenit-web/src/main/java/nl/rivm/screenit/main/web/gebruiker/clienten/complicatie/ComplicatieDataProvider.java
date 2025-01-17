package nl.rivm.screenit.main.web.gebruiker.clienten.complicatie;

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

import java.util.Collections;
import java.util.Iterator;

import nl.rivm.screenit.model.colon.Complicatie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;

public class ComplicatieDataProvider extends SortableDataProvider<Complicatie, String>
{

	public ComplicatieDataProvider(IModel<Complicatie> criteria)
	{
		Injector.get().inject(this);
		setSort("datum", SortOrder.DESCENDING);
	}

	@Override
	public Iterator<? extends Complicatie> iterator(long first, long count)
	{
		return Collections.emptyIterator();
	}

	@Override
	public long size()
	{
		return 0;
	}

	@Override
	public IModel<Complicatie> model(Complicatie object)
	{
		return ModelUtil.sModel(object);
	}

}
