
package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class ClientVerslagenDataProvider<V extends Verslag<?, ?>> extends SortableDataProvider<V, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<? extends V> zoekModel;

	@SpringBean
	private VerslagService verslagService;

	public ClientVerslagenDataProvider(IModel<? extends V> zoekModel)
	{
		this.zoekModel = zoekModel;
		setSort("datumVerwerkt", SortOrder.ASCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends V> iterator(long first, long count)
	{
		return verslagService.zoekVerslagen(ModelUtil.nullSafeGet(zoekModel), Ints.checkedCast(first), Ints.checkedCast(count), getSort().getProperty(), getSort().isAscending())
			.iterator();
	}

	@Override
	public long size()
	{
		return verslagService.countVerslagen(ModelUtil.nullSafeGet(zoekModel));
	}

	@Override
	public IModel<V> model(V object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
