package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class MammaMammografenProvider extends SortableDataProvider<MammaMammograaf, String>
{

	private static final long serialVersionUID = 1L;

	private IModel<List<MammaMammograaf>> mammografenModel;

	MammaMammografenProvider(PropertyModel<List<MammaMammograaf>> mammografenModel)
	{
		this.mammografenModel = mammografenModel;
	}

	@Override
	public Iterator<? extends MammaMammograaf> iterator(long first, long count)
	{
		final List<MammaMammograaf> mammografen = new ArrayList<>(mammografenModel.getObject()); 
		mammografen.sort(Comparator.comparing(MammaMammograaf::getAeTitle));
		return mammografen.iterator();
	}

	@Override
	public long size()
	{
		return mammografenModel.getObject().size();
	}

	@Override
	public IModel<MammaMammograaf> model(MammaMammograaf mammograaf)
	{
		return ModelUtil.sModel(mammograaf);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(mammografenModel);
	}
}
