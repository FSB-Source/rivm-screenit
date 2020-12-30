package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class WebVrijSlot extends VrijSlot implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<Kamer> locatie;

	@Override
	public Kamer getLocatie()
	{
		return ModelUtil.nullSafeGet(locatie);
	}

	@Override
	public void setLocatie(Kamer locatie)
	{
		this.locatie = ModelUtil.nullSafeSet(locatie);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(locatie);
	}
}
