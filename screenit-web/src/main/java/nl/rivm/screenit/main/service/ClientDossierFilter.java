package nl.rivm.screenit.main.service;

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
import java.util.List;

import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public class ClientDossierFilter implements Serializable, IBevolkingsonderzoek
{

	private static final long serialVersionUID = 1L;

	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	private Boolean laatsteRondes;

	public ClientDossierFilter(List<Bevolkingsonderzoek> bevolkingsonderzoeken, Boolean laatsteRondes)
	{
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
		this.laatsteRondes = laatsteRondes;
	}

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	@Override
	public void setBevolkingsonderzoeken(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
	}

	public Boolean getLaatsteRondes()
	{
		return laatsteRondes;
	}

	public void setLaatsteRondes(Boolean laatsteRondes)
	{
		this.laatsteRondes = laatsteRondes;
	}

	@Override
	public Boolean getExactMatch()
	{

		return null;
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{

	}
}
