
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats;

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

import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.primitives.Ints;

public class MammaStandplaatsDataProvider extends SortableDataProvider<MammaStandplaats, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaStandplaatsService standplaatsService;

	private IModel<MammaStandplaats> criteria;

	public MammaStandplaatsDataProvider(String sortProperty, IModel<MammaStandplaats> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends MammaStandplaats> iterator(long first, long count)
	{
		MammaStandplaats zoekObject = getZoekObject();

		return standplaatsService.zoekStandplaatsen(zoekObject, Ints.checkedCast(first), Ints.checkedCast(count), getSort().getProperty(), getSort().isAscending()).iterator();
	}

	private MammaStandplaats getZoekObject()
	{
		return ModelUtil.nullSafeGet(criteria);
	}

	@Override
	public long size()
	{
		MammaStandplaats zoekObject = getZoekObject();
		return standplaatsService.countStandplaatsen(zoekObject);
	}

	@Override
	public IModel<MammaStandplaats> model(MammaStandplaats object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}
}
