package nl.rivm.screenit.main.web.gebruiker.algemeen.projectvragenlijsten;

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

import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.rivm.screenit.service.VragenlijstBaseService;
import nl.topicuszorg.wicket.search.HibernateDataProvider;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class VragenlijstenProvider<T extends Vragenlijst> extends HibernateDataProvider<T>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private VragenlijstBaseService vragenlijstBaseService;

	public VragenlijstenProvider(IModel<T> criteriaModel, String defaultSortProperty)
	{
		super(criteriaModel, defaultSortProperty);
	}

	@Override
	public Iterator<T> iterator(long first, long count)
	{
		String sortProperty = null;
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		return vragenlijstBaseService.searchVragenlijsten(getSearchObject(), first, count, sortProperty, asc).iterator();
	}

	@Override
	public long size()
	{
		return vragenlijstBaseService.countVragenlijsten(getSearchObject());
	}
}
