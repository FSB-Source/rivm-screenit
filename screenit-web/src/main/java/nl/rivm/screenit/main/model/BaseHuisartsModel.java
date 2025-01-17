package nl.rivm.screenit.main.model;

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

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public abstract class BaseHuisartsModel<H extends HibernateObject> implements IDetachable
{
	private final IModel<H> huisartsModel;

	public BaseHuisartsModel(H huisarts)
	{
		huisartsModel = ModelUtil.sModel(huisarts);
	}

	protected H getHuisarts()
	{
		return ModelUtil.nullSafeGet(huisartsModel);
	}

	public abstract String getHuisartsNaam();

	public abstract String getPraktijkAdres();

	public abstract String getPraktijkNaam();

	public abstract String getHuisartsAgb();

	public abstract String getWeergaveNaam();

	public abstract String getKlantnummer();

	public abstract String getEdiadres();

	public abstract String getCommunicatieadres();

	public abstract boolean isVerwijderd();

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(huisartsModel);
	}

}
