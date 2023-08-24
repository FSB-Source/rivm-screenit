package nl.rivm.screenit.main.service;

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

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class BerichtenZoekFilter extends BerichtZoekFilter implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<ScreeningOrganisatie> screeningOrganisatie;

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return ModelUtil.nullSafeGet(screeningOrganisatie);
	}

	@Override
	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = ModelUtil.sModel(screeningOrganisatie);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(screeningOrganisatie);
	}

}
