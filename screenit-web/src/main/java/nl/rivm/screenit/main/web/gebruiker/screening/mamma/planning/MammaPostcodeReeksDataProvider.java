
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.impl.MammaPostcodeReeksDataProviderServiceImpl;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaPostcodeReeksDataProvider extends SortableDataProvider<MammaPostcodeReeks, String>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaPostcodeReeksDataProviderServiceImpl postcodeReeksDataProvideService;

	private IModel<MammaPostcodeReeks> criteria;

	public MammaPostcodeReeksDataProvider(String sortProperty, IModel<MammaPostcodeReeks> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends MammaPostcodeReeks> iterator(long first, long count)
	{
		var zoekObject = getZoekObject();
		return postcodeReeksDataProvideService.findPage(first, count, zoekObject, getSort()).iterator();
	}

	private MammaPostcodeReeks getZoekObject()
	{
		var zoekObject = ModelUtil.nullSafeGet(criteria);
		var ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		if (ingelogdNamensRegio != null && zoekObject.getStandplaats() == null)
		{
			zoekObject.setStandplaats(new MammaStandplaats());
			zoekObject.getStandplaats().setRegio(ingelogdNamensRegio);
		}

		return zoekObject;
	}

	@Override
	public long size()
	{
		var zoekObject = getZoekObject();
		return postcodeReeksDataProvideService.size(zoekObject);
	}

	@Override
	public IModel<MammaPostcodeReeks> model(MammaPostcodeReeks object)
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
