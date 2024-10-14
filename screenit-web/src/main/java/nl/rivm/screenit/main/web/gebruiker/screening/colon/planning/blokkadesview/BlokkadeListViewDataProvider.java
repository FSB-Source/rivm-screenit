
package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.blokkadesview;

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

import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BlokkadeListViewDataProvider extends SortableDataProvider<ColonBlokkade, String>
{

	private final IModel<RoosterListViewFilter> zoekModel;

	private final IModel<ColonIntakelocatie> intakelocatie;

	@SpringBean
	private RoosterService roosterService;

	public BlokkadeListViewDataProvider(IModel<RoosterListViewFilter> zoekModel, ColonIntakelocatie intakelocatie)
	{
		this.zoekModel = zoekModel;
		this.intakelocatie = ModelUtil.sModel(intakelocatie);
		setSort("vanaf", SortOrder.ASCENDING);
		Injector.get().inject(this);

	}

	@Override
	public Iterator<ColonBlokkade> iterator(long first, long count)
	{
		return roosterService.getBlokkades(getSort().getProperty(), getSort().isAscending(), first, count, zoekModel.getObject(),
			intakelocatie.getObject()).iterator();
	}

	@Override
	public long size()
	{
		return roosterService.getBlokkadesCount(zoekModel.getObject(), intakelocatie.getObject());
	}

	@Override
	public IModel<ColonBlokkade> model(ColonBlokkade object)
	{
		return ModelUtil.sModel(object);
	}

}
