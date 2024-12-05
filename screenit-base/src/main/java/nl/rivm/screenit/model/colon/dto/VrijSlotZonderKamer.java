package nl.rivm.screenit.model.colon.dto;

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
import java.math.BigInteger;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class VrijSlotZonderKamer implements Serializable
{
	private Date startTijd;

	private Date eindTijd;

	private Long intakelocatieId;

	private String plaats;

	private Double afstand;

	public void setIntakelocatieId(BigInteger intakelocatieId)
	{
		if (intakelocatieId != null)
		{
			this.intakelocatieId = intakelocatieId.longValue();
		}
		else
		{
			this.intakelocatieId = null;
		}
	}

	public void setNaam(String naam)
	{
	}

	public String getNaam()
	{
		return null;
	}
}
