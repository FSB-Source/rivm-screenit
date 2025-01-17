package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import java.util.Iterator;

import nl.rivm.screenit.main.service.cervix.impl.CervixHuisartsLocatieDataProviderServiceImpl;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixHuisartsLocatieProvider extends SortableDataProvider<CervixHuisartsLocatie, String>
{
	@SpringBean
	private CervixHuisartsLocatieDataProviderServiceImpl huisartsLocatieDataProviderService;

	private CervixHuisartsLocatieFilter filter;

	public CervixHuisartsLocatieProvider(CervixHuisartsLocatieFilter filter)
	{
		setSort("huisarts.organisatieMedewerkers.medewerker.achternaam", SortOrder.ASCENDING);
		this.filter = filter;
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends CervixHuisartsLocatie> iterator(long first, long count)
	{
		var sort = WicketSpringDataUtil.toSpringSort(getSort());
		return huisartsLocatieDataProviderService.findPage(first, count, filter, sort).iterator();
	}

	@Override
	public long size()
	{
		return huisartsLocatieDataProviderService.size(filter);
	}

	@Override
	public IModel<CervixHuisartsLocatie> model(CervixHuisartsLocatie huisartsLocatie)
	{
		return ModelUtil.sModel(huisartsLocatie);
	}
}
