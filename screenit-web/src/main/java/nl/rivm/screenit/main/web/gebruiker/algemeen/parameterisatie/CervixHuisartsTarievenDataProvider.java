package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixHuisartsTarievenDataProvider extends SortableDataProvider<CervixHuisartsTarief, String>
{

	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	public CervixHuisartsTarievenDataProvider()
	{
		Injector.get().inject(this);
		setSort("geldigVanafDatum", SortOrder.DESCENDING);
	}

	@Override
	public IModel<CervixHuisartsTarief> model(CervixHuisartsTarief object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public Iterator iterator(long first, long count)
	{
		return cervixVerrichtingDao.getCervixHuisartsTarieven(null, first, count, new SortState<String>(getSort().getProperty(), getSort().isAscending())).iterator();
	}

	@Override
	public long size()
	{
		return cervixVerrichtingDao.countCervixHuisartsTarieven(null);
	}

}
