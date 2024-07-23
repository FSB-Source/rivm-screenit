package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

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

import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLaboratoriumTarievenDataProvider extends SortableDataProvider<CervixLabTarief, String>
{

	@SpringBean
	private CervixVerrichtingService verrichtingService;

	private final IModel<CervixLabTarief> zoekObject;

	public CervixLaboratoriumTarievenDataProvider(IModel<CervixLabTarief> labTarief)
	{
		Injector.get().inject(this);
		setSort("geldigVanafDatum", SortOrder.DESCENDING);
		this.zoekObject = labTarief;
	}

	@Override
	public Iterator<? extends CervixLabTarief> iterator(long first, long count)
	{
		return verrichtingService.getLabTarieven(ModelUtil.nullSafeGet(zoekObject).getBmhkLaboratorium(), first, count, WicketSpringDataUtil.toSpringSort(getSort())).iterator();
	}

	@Override
	public long size()
	{
		return verrichtingService.countLabTarieven(ModelUtil.nullSafeGet(zoekObject).getBmhkLaboratorium());
	}

	@Override
	public IModel<CervixLabTarief> model(CervixLabTarief object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(zoekObject);
	}
}
