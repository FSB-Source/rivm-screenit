package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

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

import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.topicuszorg.wicket.search.HibernateDataProvider;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaDownloadOnderzoekenVerzoekenProvider extends HibernateDataProvider<MammaDownloadOnderzoekenVerzoek>
{
	@SpringBean
	private MammaUitwisselportaalService uitwisselportaalService;

	public MammaDownloadOnderzoekenVerzoekenProvider(IModel<MammaDownloadOnderzoekenVerzoek> filter)
	{
		super(filter, "aangemaaktOp");
		setSort("aangemaaktOp", SortOrder.DESCENDING);
	}

	@Override
	public Iterator<MammaDownloadOnderzoekenVerzoek> iterator(long first, long count)
	{
		String sortProperty = null;
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		return uitwisselportaalService.searchVerzoeken(getSearchObject(), first, count, sortProperty, asc).iterator();
	}

	@Override
	public long size()
	{
		return uitwisselportaalService.countVerzoeken(getSearchObject());
	}

}
