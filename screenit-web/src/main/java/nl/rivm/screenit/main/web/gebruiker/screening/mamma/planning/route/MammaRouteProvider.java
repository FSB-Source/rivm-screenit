
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.model.IModel;

public class MammaRouteProvider extends SortableDataProvider<MammaStandplaatsPeriode, String>
{

	private static final long serialVersionUID = 1L;

	IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	public MammaRouteProvider(IModel<MammaScreeningsEenheid> model)
	{
		screeningsEenheidModel = model;
	}

	@Override
	public Iterator<? extends MammaStandplaatsPeriode> iterator(long first, long count)
	{
		return screeningsEenheidModel.getObject().getStandplaatsPerioden().iterator();
	}

	@Override
	public long size()
	{
		return screeningsEenheidModel.getObject().getStandplaatsPerioden().size();
	}

	@Override
	public IModel<MammaStandplaatsPeriode> model(MammaStandplaatsPeriode standplaatsPeriode)
	{
		return ModelUtil.sModel(standplaatsPeriode);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel);
	}
}
