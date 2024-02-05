
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging;

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

import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class LoggingDataProvider extends SortableDataProvider<LogRegel, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private LogService logService;

	private final IModel<LoggingZoekCriteria> criteria;

	public LoggingDataProvider(IModel<LoggingZoekCriteria> criteria)
	{
		Injector.get().inject(this);
		setSort("gebeurtenisDatum", SortOrder.DESCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends LogRegel> iterator(long first, long count)
	{
		return logService.getLogRegels(ModelUtil.nullSafeGet(criteria), (int) first, (int) count, new SortState<>(getSort().getProperty(), getSort().isAscending())).iterator();
	}

	@Override
	public long size()
	{
		return logService.countLogRegels(ModelUtil.nullSafeGet(criteria));
	}

	@Override
	public IModel<LogRegel> model(LogRegel object)
	{
		return new SimpleHibernateModel<LogRegel>(object);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(criteria);
	}
}
