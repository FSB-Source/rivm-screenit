package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class VrijSlotZonderKamerDataProvider extends SortableDataProvider<VrijSlotZonderKamer, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private PlanningService planningService;

	private final VrijSlotZonderKamerFilter filter;

	private final IModel<Client> client;

	public VrijSlotZonderKamerDataProvider(VrijSlotZonderKamerFilter filter, IModel<Client> client)
	{
		Injector.get().inject(this);
		this.filter = filter;
		this.client = client;
	}

	@Override
	public Iterator<? extends VrijSlotZonderKamer> iterator(long first, long count)
	{
		String sortProperty = "vanaf";
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		if (filter.getAlleenIntakelocaties() && sortProperty.equals("vanaf"))
		{
			sortProperty = "naam";
			asc = true;
		}
		return planningService.getVrijeSlotenZonderKamer(sortProperty, asc, first, count, filter, client.getObject()).iterator();
	}

	@Override
	public long size()
	{
		return planningService.getVrijeSlotenZonderKamerCount(filter, client.getObject());
	}

	@Override
	public IModel<VrijSlotZonderKamer> model(VrijSlotZonderKamer vrijSlotZonderKamer)
	{
		return Model.of(vrijSlotZonderKamer);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(client);
	}
}
