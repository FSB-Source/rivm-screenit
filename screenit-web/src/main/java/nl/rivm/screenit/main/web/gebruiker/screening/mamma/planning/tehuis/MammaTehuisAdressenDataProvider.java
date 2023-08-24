package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.springframework.beans.support.PropertyComparator;

public class MammaTehuisAdressenDataProvider extends SortableDataProvider<MammaTehuisAdres, String>
{

	private IModel<MammaTehuis> tehuis;

	public MammaTehuisAdressenDataProvider(String sortProperty, IModel<MammaTehuis> tehuis)
	{
		Injector.get().inject(this);
		this.tehuis = tehuis;
		setSort(sortProperty, SortOrder.ASCENDING);
	}

	@Override
	public Iterator<? extends MammaTehuisAdres> iterator(long first, long count)
	{

		return getTehuisAdressenSorted().subList((int) first, (int) (first + count)).iterator();
	}

	private List<MammaTehuisAdres> getTehuisAdressenSorted()
	{
		List<MammaTehuisAdres> tehuisAdressen = tehuis.getObject().getAdressen();
		SortParam<String> sortParam = getSort();
		if (sortParam != null && sortParam.getProperty() != null)
		{
			tehuisAdressen = tehuisAdressen.stream().sorted(new PropertyComparator<>(sortParam.getProperty(), true, sortParam.isAscending())).collect(Collectors.toList());
		}
		return tehuisAdressen;
	}

	@Override
	public long size()
	{
		return tehuis.getObject().getAdressen().size();
	}

	@Override
	public IModel<MammaTehuisAdres> model(MammaTehuisAdres adres)
	{
		return ModelUtil.sModel(adres);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(tehuis);
	}
}
