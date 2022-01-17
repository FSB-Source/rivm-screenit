package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;

public final class PlanningBenodigdJaar
{
	private final Integer jaar;

	private BigDecimal totaal;

	private BigDecimal totaalTehuis;

	private BigDecimal nieuw;

	private BigDecimal nieuwTehuis;

	private BigDecimal oud;

	private BigDecimal oudTehuis;

	private BigDecimal eersteOnderzoekCorrectie;

	public PlanningBenodigdJaar(Integer jaar)
	{
		clear();
		this.jaar = jaar;
	}

	public Integer getJaar()
	{
		return jaar;
	}

	public void add(BigDecimal benodigd, boolean tehuis)
	{
		if (tehuis)
		{
			totaalTehuis = totaalTehuis.add(benodigd);
		}

		totaal = totaal.add(benodigd);
	}

	public void addToNieuw(BigDecimal benodigd, boolean tehuis)
	{
		if (tehuis)
		{
			nieuwTehuis = nieuwTehuis.add(benodigd);
		}

		nieuw = nieuw.add(benodigd);
	}

	public void addToOud(BigDecimal benodigd, boolean tehuis)
	{
		if (tehuis)
		{
			oudTehuis = oudTehuis.add(benodigd);
		}

		oud = oud.add(benodigd);
	}

	public void addToEersteOnderzoekCorrectie(BigDecimal benodigd)
	{
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.add(benodigd);
	}

	public void add(PlanningBenodigdJaar benodigdJaar)
	{
		totaal = totaal.add(benodigdJaar.totaal);
		totaalTehuis = totaalTehuis.add(benodigdJaar.totaalTehuis);
		nieuw = nieuw.add(benodigdJaar.nieuw);
		nieuwTehuis = nieuwTehuis.add(benodigdJaar.nieuwTehuis);
		oud = oud.add(benodigdJaar.oud);
		oudTehuis = oudTehuis.add(benodigdJaar.oudTehuis);
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.add(benodigdJaar.getEersteOnderzoekCorrectie());
	}

	public void subtract(PlanningBenodigdJaar benodigdJaar)
	{
		totaal = totaal.subtract(benodigdJaar.totaal);
		totaalTehuis = totaalTehuis.subtract(benodigdJaar.totaalTehuis);
		nieuw = nieuw.subtract(benodigdJaar.nieuw);
		nieuwTehuis = nieuwTehuis.subtract(benodigdJaar.nieuwTehuis);
		oud = oud.subtract(benodigdJaar.oud);
		oudTehuis = oudTehuis.subtract(benodigdJaar.oudTehuis);
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.subtract(benodigdJaar.getEersteOnderzoekCorrectie());
	}

	public void clear()
	{
		totaal = BigDecimal.ZERO;
		totaalTehuis = BigDecimal.ZERO;
		nieuw = BigDecimal.ZERO;
		nieuwTehuis = BigDecimal.ZERO;
		oud = BigDecimal.ZERO;
		oudTehuis = BigDecimal.ZERO;
		eersteOnderzoekCorrectie = BigDecimal.ZERO;
	}

	public BigDecimal getTotaal()
	{
		return totaal;
	}

	public BigDecimal getTotaalTehuis()
	{
		return totaalTehuis;
	}

	public BigDecimal getNieuw()
	{
		return nieuw;
	}

	public BigDecimal getNieuwTehuis()
	{
		return nieuwTehuis;
	}

	public BigDecimal getOud()
	{
		return oud;
	}

	public BigDecimal getOudTehuis()
	{
		return oudTehuis;
	}

	private void setTotaal(BigDecimal totaal)
	{
		this.totaal = totaal;
	}

	private void setTotaalTehuis(BigDecimal totaalTehuis)
	{
		this.totaalTehuis = totaalTehuis;
	}

	private void setNieuw(BigDecimal nieuw)
	{
		this.nieuw = nieuw;
	}

	private void setNieuwTehuis(BigDecimal nieuwTehuis)
	{
		this.nieuwTehuis = nieuwTehuis;
	}

	public void setOud(BigDecimal oud)
	{
		this.oud = oud;
	}

	public void setOudTehuis(BigDecimal oudTehuis)
	{
		this.oudTehuis = oudTehuis;
	}

	public BigDecimal getEersteOnderzoekCorrectie()
	{
		return eersteOnderzoekCorrectie;
	}

	public void setEersteOnderzoekCorrectie(BigDecimal eersteOnderzoekCorrectie)
	{
		this.eersteOnderzoekCorrectie = eersteOnderzoekCorrectie;
	}
}
