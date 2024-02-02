package nl.rivm.screenit.model.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;

import nl.rivm.screenit.model.enums.BezwaarType;

public class BezwaarViewWrapper implements Serializable
{

	private static final long serialVersionUID = 1L;

	private BezwaarType type;

	private Boolean actief;

	private String resourceKey;

	public BezwaarType getType()
	{
		return type;
	}

	public void setType(BezwaarType type)
	{
		this.type = type;
	}

	public Boolean getActief()
	{
		return actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public String getNaam()
	{
		return getType().getNaam();
	}

	public String getResourceKey()
	{
		return resourceKey;
	}

	public void setResourceKey(String resourceKey)
	{
		this.resourceKey = resourceKey;
	}

}
