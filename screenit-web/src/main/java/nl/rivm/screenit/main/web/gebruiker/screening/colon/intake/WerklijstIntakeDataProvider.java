
package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class WerklijstIntakeDataProvider extends SortableDataProvider<ColonIntakeAfspraak, String>
{

	private final IModel<WerklijstIntakeFilter> zoekModel;

	@SpringBean
	private ColonBaseAfspraakService afspraakService;

	private final IModel<ColoscopieCentrum> coloscopieCentrum;

	public WerklijstIntakeDataProvider(IModel<WerklijstIntakeFilter> zoekModel, ColoscopieCentrum coloscopieCentrum)
	{
		this.zoekModel = zoekModel;
		this.coloscopieCentrum = ModelUtil.sModel(coloscopieCentrum);
		setSort("startTime", SortOrder.ASCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends ColonIntakeAfspraak> iterator(long first, long count)
	{
		return afspraakService.getAfsprakenVoorColoscopiecentrum(ModelUtil.nullSafeGet(zoekModel), ModelUtil.nullSafeGet(coloscopieCentrum),
			first, count, getSort().getProperty(), getSort().isAscending()).iterator();
	}

	@Override
	public long size()
	{
		return afspraakService.countAfsprakenVoorColoscopiecentrum(ModelUtil.nullSafeGet(zoekModel), ModelUtil.nullSafeGet(coloscopieCentrum));
	}

	@Override
	public IModel<ColonIntakeAfspraak> model(ColonIntakeAfspraak object)
	{
		return ModelUtil.sModel(object);
	}

}
