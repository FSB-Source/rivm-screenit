package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaPortfolioZoekObject;
import nl.rivm.screenit.main.service.mamma.impl.MammaPortfolioDataProviderServiceImpl;
import nl.rivm.screenit.model.Client;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaPortfolioDataProvider extends SortableDataProvider<Client, String>
{
	@SpringBean
	private MammaPortfolioDataProviderServiceImpl portfolioDataProviderService;

	private IModel<MammaPortfolioZoekObject> zoekObjectModel;

	public MammaPortfolioDataProvider(String sortProperty, IModel<MammaPortfolioZoekObject> zoekObjectModel)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.zoekObjectModel = zoekObjectModel;
	}

	@Override
	public Iterator<? extends Client> iterator(long first, long count)
	{
		return portfolioDataProviderService.findPage(first, count, getZoekObject(), getSort()).iterator();
	}

	private MammaPortfolioZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(zoekObjectModel);
	}

	@Override
	public long size()
	{
		return portfolioDataProviderService.size(getZoekObject());
	}

	@Override
	public IModel<Client> model(Client object)
	{
		return ModelUtil.sModel(object);
	}

}
