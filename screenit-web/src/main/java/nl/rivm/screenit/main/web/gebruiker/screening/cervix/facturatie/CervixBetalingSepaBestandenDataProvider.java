package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

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

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixBetalingSepaBestandenDataProvider extends SortableDataProvider<CervixBetaalopdracht, String>
{
	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	public CervixBetalingSepaBestandenDataProvider()
	{
		Injector.get().inject(this);
		setSort("statusDatum", SortOrder.DESCENDING);
	}

	@Override
	public Iterator<? extends CervixBetaalopdracht> iterator(long first, long count)
	{
		return cervixVerrichtingDao.getBetaalOpdrachten(new SortState<>(getSort().getProperty(), getSort().isAscending()), first, count).iterator();
	}

	@Override
	public long size()
	{
		return cervixVerrichtingDao.countBetaalOpdrachten();
	}

	@Override
	public IModel<CervixBetaalopdracht> model(CervixBetaalopdracht cervixBetaalopdracht)
	{
		return ModelUtil.sModel(cervixBetaalopdracht);
	}

	@Override
	public void detach()
	{
		super.detach();
	}
}
