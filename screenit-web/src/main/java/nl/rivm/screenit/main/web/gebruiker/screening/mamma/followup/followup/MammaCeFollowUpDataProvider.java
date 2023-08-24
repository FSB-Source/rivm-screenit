package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followup;

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

import com.google.common.primitives.Ints;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaCeWerklijstService;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.Iterator;

public class MammaCeFollowUpDataProvider extends SortableDataProvider<MammaBeoordeling, String>
{

	@SpringBean
	private MammaCeWerklijstService ceWerklijstService;

	private IModel<MammaCeWerklijstZoekObject> criteria;

	MammaCeFollowUpDataProvider(String sortProperty, IModel<MammaCeWerklijstZoekObject> criteria)
	{
		Injector.get().inject(this);
		setSort(sortProperty, SortOrder.ASCENDING);
		this.criteria = criteria;
	}

	@Override
	public Iterator<? extends MammaBeoordeling> iterator(long first, long count)
	{
		return ceWerklijstService.zoekFollowUpBeoordelingen(getZoekObject(), Ints.checkedCast(first), Ints.checkedCast(count), getSort().getProperty(), getSort().isAscending())
			.iterator();
	}

	@Override
	public long size()
	{
		return ceWerklijstService.countFollowUpBeoordelingen(getZoekObject());
	}

	@Override
	public IModel<MammaBeoordeling> model(MammaBeoordeling mammaBeoordeling)
	{
		return ModelUtil.sModel(mammaBeoordeling);
	}

	private MammaCeWerklijstZoekObject getZoekObject()
	{
		return ModelUtil.nullSafeGet(criteria);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteria);
	}
}
