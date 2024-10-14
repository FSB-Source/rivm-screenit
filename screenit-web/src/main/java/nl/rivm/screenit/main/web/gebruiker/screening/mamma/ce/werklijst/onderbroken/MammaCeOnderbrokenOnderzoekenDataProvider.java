package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.onderbroken;

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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.impl.MammaCeOnderbrokenOnderzoekenDataProviderServiceImpl;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaCeOnderbrokenOnderzoekenDataProvider extends SortableDataProvider<MammaOnderzoek, String>
{

	@SpringBean
	private MammaCeOnderbrokenOnderzoekenDataProviderServiceImpl dataProviderService;

	private IModel<MammaCeWerklijstZoekObject> criteria;

	MammaCeOnderbrokenOnderzoekenDataProvider(String sortProperty, IModel<MammaCeWerklijstZoekObject> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends MammaOnderzoek> iterator(long first, long count)
	{
		return dataProviderService.findPage(first, count, getZoekObject(), getSort()).iterator();
	}

	@Override
	public long size()
	{
		return dataProviderService.size(getZoekObject());
	}

	private MammaCeWerklijstZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(criteria);
	}

	@Override
	public IModel<MammaOnderzoek> model(MammaOnderzoek object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}
}
