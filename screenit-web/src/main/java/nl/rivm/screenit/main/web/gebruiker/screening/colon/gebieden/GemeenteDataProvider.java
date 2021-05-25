
package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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

import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.service.GemeenteService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GemeenteDataProvider extends SortableDataProvider<Gemeente, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<Gemeente> criteriaModel;

	@SpringBean
	private GemeenteService gemeenteService;

	public GemeenteDataProvider(IModel<Gemeente> criteriaModel, String sortProperty)
	{
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteriaModel = criteriaModel;
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends Gemeente> iterator(long first, long count)
	{
		return gemeenteService.zoekGemeentes(ModelUtil.nullSafeGet(criteriaModel), first, count, getSort().getProperty(), getSort().isAscending()).iterator();
	}

	@Override
	public long size()
	{
		return gemeenteService.countGemeentes(ModelUtil.nullSafeGet(criteriaModel));
	}

	@Override
	public IModel<Gemeente> model(Gemeente object)
	{
		return ModelUtil.sModel(object);
	}

}
