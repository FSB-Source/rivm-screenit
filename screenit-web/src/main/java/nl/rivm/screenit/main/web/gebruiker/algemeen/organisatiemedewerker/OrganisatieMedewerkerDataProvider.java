package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OrganisatieMedewerkerDataProvider extends SortableDataProvider<InstellingGebruiker, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MedewerkerService medewerkerService;

	private IModel<InstellingGebruiker> searchObjectModel;

	public OrganisatieMedewerkerDataProvider(IModel<InstellingGebruiker> searchObjectModel, String defaultSortProperty)
	{
		setSort(new SortParam<String>(defaultSortProperty, true));
		this.searchObjectModel = searchObjectModel;
		Injector.get().inject(this);
	}

	@Override
	public long size()
	{
		return medewerkerService.countInstellingGebruiker(ModelUtil.nullSafeGet(searchObjectModel));
	}

	@Override
	public Iterator<? extends InstellingGebruiker> iterator(long first, long count)
	{

		String sortProperty = null;
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		return medewerkerService.searchInstellingGebruiker(ModelUtil.nullSafeGet(searchObjectModel), first, count, sortProperty, asc).iterator();
	}

	@Override
	public IModel<InstellingGebruiker> model(InstellingGebruiker object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(searchObjectModel);
	}

}
