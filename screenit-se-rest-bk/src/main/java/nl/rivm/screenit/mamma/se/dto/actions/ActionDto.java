package nl.rivm.screenit.mamma.se.dto.actions;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public final class ActionDto
{
	private SEActieType type;

	private String nodeText;

	public ActionDto()
	{
	}

	public SEActieType getType()
	{
		return type;
	}

	public void setType(SEActieType type)
	{
		this.type = type;
	}

	public String getNodeText()
	{
		return nodeText;
	}

	public void setNodeText(String nodeText)
	{
		this.nodeText = nodeText;
	}
}
