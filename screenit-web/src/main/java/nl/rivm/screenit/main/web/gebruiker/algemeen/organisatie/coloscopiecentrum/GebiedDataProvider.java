
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopiecentrum;

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

import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.service.GemeenteService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GebiedDataProvider extends SortableDataProvider<UitnodigingsGebied, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<UitnodigingsGebied> zoekObjectModel;

	private final IModel<ColoscopieCentrum> coloscopieCentrum;

	@SpringBean
	private GemeenteService gemeenteService;

	public GebiedDataProvider(IModel<UitnodigingsGebied> zoekObjectModel, ColoscopieCentrum coloscopieCentrum, String sortProperty)
	{
		this.zoekObjectModel = zoekObjectModel;
		this.coloscopieCentrum = ModelUtil.sModel(coloscopieCentrum);
		setSort(sortProperty, SortOrder.ASCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends UitnodigingsGebied> iterator(long first, long count)
	{
		return gemeenteService.getGebieden(ModelUtil.nullSafeGet(zoekObjectModel), ModelUtil.nullSafeGet(coloscopieCentrum), first, count, getSort().getProperty(),
			getSort().isAscending());
	}

	@Override
	public long size()
	{
		return gemeenteService.getCountGebieden(ModelUtil.nullSafeGet(zoekObjectModel), ModelUtil.nullSafeGet(coloscopieCentrum));
	}

	@Override
	public IModel<UitnodigingsGebied> model(UitnodigingsGebied object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
		ModelUtil.nullSafeDetach(coloscopieCentrum);
	}

}
