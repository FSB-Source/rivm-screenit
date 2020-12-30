package nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class VerwerkteBerichtenOverzichtDataProvider extends SortableDataProvider<OntvangenCdaBericht, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private VerslagService verslagService;

	private final IModel<BerichtZoekFilter> filter;

	public VerwerkteBerichtenOverzichtDataProvider(IModel<BerichtZoekFilter> filter)
	{
		setSort("ontvangen", SortOrder.DESCENDING);
		this.filter = filter;
		Injector.get().inject(this);

	}

	@Override
	public Iterator<OntvangenCdaBericht> iterator(long first, long count)
	{
		List<OntvangenCdaBericht> searchOngeldigeBerichten = verslagService.searchBerichten(ModelUtil.nullSafeGet(filter), first, count, getSort().getProperty(),
			getSort().isAscending());
		return searchOngeldigeBerichten.iterator();
	}

	@Override
	public long size()
	{
		return verslagService.countBerichten(ModelUtil.nullSafeGet(filter));
	}

	@Override
	public IModel<OntvangenCdaBericht> model(OntvangenCdaBericht object)
	{
		return ModelUtil.sModel(object);
	}

}
