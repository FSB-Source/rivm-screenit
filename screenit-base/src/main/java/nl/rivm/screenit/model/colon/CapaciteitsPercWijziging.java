
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.math.BigDecimal;

public class CapaciteitsPercWijziging implements Serializable
{

	private static final long serialVersionUID = 1L;

	private Long ilUgId;

	private Long ilId;

	private String intakelocatie;

	private Long ugId;

	private String uitnodigingsgebied;

	private Integer oudCapPer;

	private BigDecimal oudBerekendeIntakes;

	private BigDecimal oudIntakesProg;

	private Integer nieuwCapPer;

	private BigDecimal nieuwBerekendeIntakes;

	private BigDecimal nieuwIntakesProg;

	private Integer oudAdhPer;

	private Integer nieuwAdhPer;

	public Long getIlUgId()
	{
		return ilUgId;
	}

	public void setIlUgId(Long ilUgId)
	{
		this.ilUgId = ilUgId;
	}

	public Long getIlId()
	{
		return ilId;
	}

	public void setIlId(Long ilId)
	{
		this.ilId = ilId;
	}

	public String getIntakelocatie()
	{
		return intakelocatie;
	}

	public void setIntakelocatie(String intakelocatie)
	{
		this.intakelocatie = intakelocatie;
	}

	public Long getUgId()
	{
		return ugId;
	}

	public void setUgId(Long ugId)
	{
		this.ugId = ugId;
	}

	public String getUitnodigingsgebied()
	{
		return uitnodigingsgebied;
	}

	public void setUitnodigingsgebied(String uitnodigingsgebied)
	{
		this.uitnodigingsgebied = uitnodigingsgebied;
	}

	public Integer getOudCapPer()
	{
		return oudCapPer;
	}

	public void setOudCapPer(Integer oudCapPer)
	{
		this.oudCapPer = oudCapPer;
	}

	public BigDecimal getOudBerekendeIntakes()
	{
		return oudBerekendeIntakes;
	}

	public void setOudBerekendeIntakes(BigDecimal oudBerekendeIntakes)
	{
		this.oudBerekendeIntakes = oudBerekendeIntakes;
	}

	public BigDecimal getOudIntakesProg()
	{
		return oudIntakesProg;
	}

	public void setOudIntakesProg(BigDecimal oudIntakesProg)
	{
		this.oudIntakesProg = oudIntakesProg;
	}

	public Integer getNieuwCapPer()
	{
		return nieuwCapPer;
	}

	public void setNieuwCapPer(Integer nieuwCapPer)
	{
		this.nieuwCapPer = nieuwCapPer;
	}

	public BigDecimal getNieuwBerekendeIntakes()
	{
		return nieuwBerekendeIntakes;
	}

	public void setNieuwBerekendeIntakes(BigDecimal nieuwBerekendeIntakes)
	{
		this.nieuwBerekendeIntakes = nieuwBerekendeIntakes;
	}

	public BigDecimal getNieuwIntakesProg()
	{
		return nieuwIntakesProg;
	}

	public void setNieuwIntakesProg(BigDecimal nieuwIntakesProg)
	{
		this.nieuwIntakesProg = nieuwIntakesProg;
	}

	public BigDecimal getVerschilOud()
	{
		if (oudIntakesProg != null)
		{
			return oudIntakesProg.subtract(oudBerekendeIntakes);
		}
		return null;
	}

	public BigDecimal getVerschilNieuw()
	{
		if (nieuwIntakesProg != null)
		{
			return nieuwIntakesProg.subtract(nieuwBerekendeIntakes);
		}
		return null;
	}

	public Integer getOudAdhPer()
	{
		return oudAdhPer;
	}

	public void setOudAdhPer(Integer oudAdhPer)
	{
		this.oudAdhPer = oudAdhPer;
	}

	public Integer getNieuwAdhPer()
	{
		return nieuwAdhPer;
	}

	public void setNieuwAdhPer(Integer nieuwAdhPer)
	{
		this.nieuwAdhPer = nieuwAdhPer;
	}

	@Override
	public boolean equals(Object other)
	{
		CapaciteitsPercWijziging wijziging = (CapaciteitsPercWijziging) other;
		return ilId.equals(wijziging.ilId) && ugId.equals(wijziging.ugId);
	}

}
