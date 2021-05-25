
package nl.rivm.screenit.main.model;

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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.ScreeningRonde;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class ScreeningRondeGebeurtenissen implements Serializable, IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<ScreeningRonde> screeningRonde;

	private final int rondenr;

	private List<ScreeningRondeGebeurtenis> gebeurtenissen = new ArrayList<>();

	public ScreeningRondeGebeurtenissen(int rondenr)
	{
		this.rondenr = rondenr;
	}

	public ScreeningRonde getScreeningRonde()
	{
		return ModelUtil.nullSafeGet(screeningRonde);
	}

	public void setScreeningRonde(ScreeningRonde screeningRonde)
	{
		this.screeningRonde = ModelUtil.sModel(screeningRonde);
	}

	public void addGebeurtenissen(List<ScreeningRondeGebeurtenis> gebeurtenissen)
	{
		for (ScreeningRondeGebeurtenis screeningRondeGebeurtenis : gebeurtenissen)
		{
			addGebeurtenis(screeningRondeGebeurtenis);
		}
	}

	public void addGebeurtenis(ScreeningRondeGebeurtenis gebeurtenis)
	{
		if (gebeurtenis != null)
		{
			gebeurtenissen.add(gebeurtenis);
			gebeurtenis.setScreeningRondeGebeurtenissen(this);
		}
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(screeningRonde);
		for (ScreeningRondeGebeurtenis gebeurtenis : gebeurtenissen)
		{
			ModelUtil.nullSafeDetach(gebeurtenis);
		}
	}

	public int getRondenr()
	{
		return rondenr;
	}

}
