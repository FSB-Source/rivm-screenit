package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst;

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
import java.util.List;

import nl.rivm.screenit.main.service.cervix.impl.CervixLabformulierDataProviderServiceImpl;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLabformulierProvider extends SortableDataProvider<CervixLabformulier, String>
{

	@SpringBean
	private CervixLabformulierDataProviderServiceImpl labformulierService;

	private CervixLabformulierenFilter filter;

	public CervixLabformulierProvider(CervixLabformulierenFilter filter)
	{
		Injector.get().inject(this);
		this.filter = filter;
		setSort("scanDatum", SortOrder.ASCENDING);
	}

	@Override
	public Iterator<? extends CervixLabformulier> iterator(long first, long count)
	{
		return labformulierService.findPage(first, count, filter, getSort()).iterator();
	}

	@Override
	public long size()
	{
		return labformulierService.size(filter);
	}

	@Override
	public IModel<CervixLabformulier> model(CervixLabformulier labformulier)
	{
		return ModelUtil.sModel(labformulier);
	}

	public List<Long> getLabformulierenIds()
	{
		return labformulierService.getLabformulierenIds(filter, getSort().getProperty(), getSort().isAscending());
	}
}
