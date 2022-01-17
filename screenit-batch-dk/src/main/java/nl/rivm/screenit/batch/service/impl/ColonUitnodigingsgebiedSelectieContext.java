package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.StringUtil;

public class ColonUitnodigingsgebiedSelectieContext implements Comparable<ColonUitnodigingsgebiedSelectieContext>, Serializable
{

	private static final long serialVersionUID = 1L;

	private static final int MAX_COLLECTION_STRING_LENGTH = 10;

	private final Long uitnodigingsgebiedId;

	private BigDecimal uitnodigingscapaciteitOrig;

	private BigDecimal uitnodigingscapaciteitOver;

	private BigDecimal uitnodigingscapaciteitTotaal;

	private boolean leeglopendGebied;

	private Integer totaalAntalClienten = null;

	private Map<Long, BigDecimal> capVanIl = new HashMap<>();

	public ColonUitnodigingsgebiedSelectieContext(BigDecimal uitnodigingscapaciteit, Long intakelocatieId, Long uitnodigingsgebiedId)
	{
		this.uitnodigingscapaciteitTotaal = uitnodigingscapaciteit;
		this.uitnodigingscapaciteitOver = uitnodigingscapaciteit;
		this.uitnodigingsgebiedId = uitnodigingsgebiedId;
		capVanIl.put(intakelocatieId, uitnodigingscapaciteit);
	}

	public void addUitnodigingscapaciteit(BigDecimal toeTeVoegenUitnodigingscapaciteit, Long intakelocatieId, boolean initial)
	{
		if (!initial && uitnodigingscapaciteitOrig == null)
		{
			uitnodigingscapaciteitOrig = uitnodigingscapaciteitTotaal;
		}
		uitnodigingscapaciteitTotaal = uitnodigingscapaciteitTotaal.add(toeTeVoegenUitnodigingscapaciteit);
		uitnodigingscapaciteitOver = uitnodigingscapaciteitOver.add(toeTeVoegenUitnodigingscapaciteit);
		BigDecimal huidigeCapVanIL = capVanIl.get(intakelocatieId);
		if (huidigeCapVanIL == null)
		{
			huidigeCapVanIL = BigDecimal.ZERO;
		}
		capVanIl.put(intakelocatieId, huidigeCapVanIL.add(toeTeVoegenUitnodigingscapaciteit));
	}

	public BigDecimal getUitnodigingscapaciteitOverVoorIntakelocatie(Long ilId)
	{
		if (uitnodigingscapaciteitTotaal.compareTo(BigDecimal.ZERO) > 0)
		{
			BigDecimal rest = capVanIl.get(null);
			BigDecimal totaal = uitnodigingscapaciteitTotaal;
			if (rest != null && capVanIl.size() > 1)
			{

				totaal = uitnodigingscapaciteitTotaal.subtract(rest);
			}
			BigDecimal capaciteitOverVoorIL = capVanIl.get(ilId).multiply(uitnodigingscapaciteitOver).divide(totaal, 10, RoundingMode.HALF_UP);
			if (rest != null && rest.compareTo(BigDecimal.ZERO) != 0 && capVanIl.size() > 1)
			{

				capaciteitOverVoorIL = capaciteitOverVoorIL.add(rest.divide(BigDecimal.valueOf(capVanIl.size() - 1), 10, RoundingMode.HALF_UP));
			}
			return capaciteitOverVoorIL;
		}
		else
		{
			return BigDecimal.ZERO;
		}

	}

	public Long getUitnodigingsgebiedId()
	{
		return uitnodigingsgebiedId;
	}

	public BigDecimal getUitnodigingscapaciteitOver()
	{
		return uitnodigingscapaciteitOver;
	}

	public void consumeUitnodigingscapaciteit()
	{
		uitnodigingscapaciteitOver = uitnodigingscapaciteitOver.subtract(BigDecimal.ONE);
		consumeClient();
	}

	public void substractUitnodigingscapaciteit(BigDecimal capaciteitAftrekken)
	{
		uitnodigingscapaciteitOver = uitnodigingscapaciteitOver.subtract(capaciteitAftrekken);
	}

	private void consumeClient()
	{
		if (getTotaalAntalClienten() != null)
		{
			setTotaalAntalClienten(getTotaalAntalClienten() - 1);
			if (getTotaalAntalClienten() < 0)
			{
				throw new IllegalStateException("Totaal aantal clienten mogen niet lager dan 0 worden.");
			}
		}
	}

	public boolean isGenoegUitnodigingscapaciteitOver()
	{
		return uitnodigingscapaciteitOver.compareTo(BigDecimal.ONE) >= 0 && (getTotaalAntalClienten() == null || getTotaalAntalClienten() > 0);
	}

	public int getUitnodigingscapaciteitToevoegingOfOver()
	{
		BigDecimal honderd = BigDecimal.valueOf(100);
		if (uitnodigingscapaciteitOrig != null)
		{
			if (uitnodigingscapaciteitTotaal.compareTo(uitnodigingscapaciteitOrig) > 0)
			{
				if (uitnodigingscapaciteitOrig.compareTo(BigDecimal.ZERO) > 0)
				{

					return uitnodigingscapaciteitTotaal.multiply(honderd).divide(uitnodigingscapaciteitOrig, 10, RoundingMode.HALF_UP).subtract(honderd).intValue();
				}
				else
				{
					return Integer.MAX_VALUE;
				}
			}
		}
		else if (uitnodigingscapaciteitOver.compareTo(uitnodigingscapaciteitTotaal) <= 0 && uitnodigingscapaciteitTotaal.compareTo(BigDecimal.ZERO) > 0)
		{

			return -uitnodigingscapaciteitOver.multiply(honderd).divide(uitnodigingscapaciteitTotaal, 10, RoundingMode.HALF_UP).intValue();
		}
		return 0;
	}

	public void tweeClientenOpEenAdres()
	{

		consumeClient();
	}

	public boolean isLeeglopendGebied()
	{
		return leeglopendGebied;
	}

	public void setLeeglopendGebied(boolean leeglopendGebied)
	{
		this.leeglopendGebied = leeglopendGebied;
	}

	public void roundUitnodigingscapaciteitOver()
	{
		addUitnodigingscapaciteit(BigDecimal.valueOf(BigDecimalUtil.roundCapaciteit(uitnodigingscapaciteitOver)).subtract(uitnodigingscapaciteitOver), null, true);
	}

	public Integer getTotaalAntalClienten()
	{
		return totaalAntalClienten;
	}

	public void setTotaalAntalClienten(Integer totaalAntalClienten)
	{
		this.totaalAntalClienten = totaalAntalClienten;
	}

	@Override
	public int compareTo(ColonUitnodigingsgebiedSelectieContext o)
	{
		return uitnodigingsgebiedId.compareTo(o.getUitnodigingsgebiedId());
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (uitnodigingsgebiedId == null ? 0 : uitnodigingsgebiedId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (obj == null)
		{
			return false;
		}
		if (!(obj instanceof ColonUitnodigingsgebiedSelectieContext))
		{
			return false;
		}
		ColonUitnodigingsgebiedSelectieContext other = (ColonUitnodigingsgebiedSelectieContext) obj;
		if (uitnodigingsgebiedId == null)
		{
			return other.uitnodigingsgebiedId == null;
		}
		else
		{
			return uitnodigingsgebiedId.equals(other.uitnodigingsgebiedId);
		}
	}

	@Override
	public String toString()
	{
		return "ColonUitnodigingsgebiedSelectieContext [" + (uitnodigingsgebiedId != null ? "ugId=" + uitnodigingsgebiedId + ", " : "")
			+ (uitnodigingscapaciteitOrig != null ? "ucapOrig=" + BigDecimalUtil.decimalToString(uitnodigingscapaciteitOrig) + ", " : "")
			+ (uitnodigingscapaciteitOver != null ? "ucapOver=" + BigDecimalUtil.decimalToString(uitnodigingscapaciteitOver) + ", " : "")
			+ (uitnodigingscapaciteitTotaal != null ? "ucapTot=" + BigDecimalUtil.decimalToString(uitnodigingscapaciteitTotaal) + ", " : "") 
			+ (capVanIl != null ? "ucapVanIl=" + StringUtil.toString(capVanIl.entrySet(), MAX_COLLECTION_STRING_LENGTH) + ", " : "") 
			+ (totaalAntalClienten != null ? "#cl=" + totaalAntalClienten + ", " : "") 
			+ "leeglopend=" + leeglopendGebied 
			+ "]";
	}

}
