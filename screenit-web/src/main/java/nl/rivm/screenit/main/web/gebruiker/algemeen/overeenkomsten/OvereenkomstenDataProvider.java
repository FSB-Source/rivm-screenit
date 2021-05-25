
package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OvereenkomstenDataProvider extends SortableDataProvider<Overeenkomst, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<Overeenkomst> actiefModel;

	@SpringBean
	private OvereenkomstService overeenkomstService;

	public OvereenkomstenDataProvider(IModel<Overeenkomst> actiefModel)
	{
		this.actiefModel = actiefModel;
		Injector.get().inject(this);
		setSort("naam", SortOrder.ASCENDING);
	}

	@Override
	public Iterator<? extends Overeenkomst> iterator(long first, long count)
	{
		return overeenkomstService.getOvereenkomsten(actiefModel.getObject().getActief(), first, count, getSort().getProperty(), getSort().isAscending()).iterator();
	}

	@Override
	public long size()
	{
		return overeenkomstService.countOvereenkomsten(actiefModel.getObject().getActief());
	}

	@Override
	public IModel<Overeenkomst> model(Overeenkomst object)
	{
		return new SimpleHibernateModel<Overeenkomst>(object);
	}
}
