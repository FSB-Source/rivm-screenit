package nl.rivm.screenit.model.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.math.BigInteger;
import java.util.Date;

public class VrijSlotZonderKamer implements Serializable
{

	private static final long serialVersionUID = 1L;

	private Date startTijd;

	private Date eindTijd;

	private Long intakeLocatieId;

	private String plaats;

	private Double afstand;

	public Date getStartTijd()
	{
		return startTijd;
	}

	public void setStartTijd(Date startTijd)
	{
		this.startTijd = startTijd;
	}

	public Date getEindTijd()
	{
		return eindTijd;
	}

	public void setEindTijd(Date eindTijd)
	{
		this.eindTijd = eindTijd;
	}

	public Long getIntakeLocatieId()
	{
		return intakeLocatieId;
	}

	public void setIntakeLocatieId(BigInteger intakeLocatieId)
	{
		if (intakeLocatieId != null)
		{
			this.intakeLocatieId = intakeLocatieId.longValue();
		}
		else
		{
			this.intakeLocatieId = null;
		}
	}

	public String getPlaats()
	{
		return plaats;
	}

	public void setPlaats(String plaats)
	{
		this.plaats = plaats;
	}

	public Double getAfstand()
	{
		return afstand;
	}

	public void setAfstand(Double afstand)
	{
		this.afstand = afstand;
	}

	public void setNaam(String naam)
	{
	}

	public String getNaam()
	{
		return null;
	}
}
