
package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole;

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

import nl.rivm.screenit.main.model.colon.IFobtBatchFilter;
import nl.rivm.screenit.main.service.colon.IfobtBestandService;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class IfobtBestandenDataProvider extends SortableDataProvider<IFOBTBestand, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<IFobtBatchFilter> zoekModel;

	@SpringBean
	private IfobtBestandService ifobtBestandService;

	public IfobtBestandenDataProvider(IModel<IFobtBatchFilter> zoekModel)
	{
		this.zoekModel = zoekModel;
		setSort("statusDatum", SortOrder.ASCENDING);
		Injector.get().inject(this);
	}

	@Override
	public Iterator<? extends IFOBTBestand> iterator(long first, long count)
	{
		return ifobtBestandService.getBestanden(ModelUtil.nullSafeGet(zoekModel), first, count, getSort().getProperty(), getSort().isAscending());
	}

	@Override
	public long size()
	{
		return ifobtBestandService.countBestanden(ModelUtil.nullSafeGet(zoekModel));
	}

	@Override
	public IModel<IFOBTBestand> model(IFOBTBestand object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
