package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang3.StringUtils;

public class EnovationHuisartsModel extends BaseHuisartsModel<EnovationHuisarts>
{

	public EnovationHuisartsModel(EnovationHuisarts enovationHuisarts)
	{
		super(enovationHuisarts);
	}

	@Override
	public String getHuisartsNaam()
	{
		return NaamUtil.getNaamHuisarts(getHuisarts());
	}

	@Override
	public String getPraktijkNaam()
	{
		return getHuisarts().getPraktijknaam();
	}

	@Override
	public String getPraktijkAdres()
	{
		return AdresUtil.getVolledigeAdresString(getHuisarts().getAdres());
	}

	@Override
	public String getHuisartsAgb()
	{
		String agbCode = getHuisarts().getHuisartsAgb();
		if (StringUtils.isBlank(agbCode))
		{
			agbCode = getHuisarts().getPraktijkAgb();
		}
		return agbCode;
	}

	@Override
	public String getWeergaveNaam()
	{
		return getHuisarts().getWeergavenaam();
	}

	@Override
	public String getKlantnummer()
	{
		return getHuisarts().getKlantnummer();
	}

	@Override
	public String getEdiadres()
	{
		return getHuisarts().getOorspronkelijkEdiadres();
	}

	@Override
	public String getCommunicatieadres()
	{
		return getHuisarts().getEdiadres();
	}

	@Override
	public boolean isVerwijderd()
	{
		return getHuisarts().isVerwijderd();
	}

}
