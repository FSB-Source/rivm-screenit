package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "uitnodiging",
	uniqueConstraints = { @UniqueConstraint(columnNames = "brief"), @UniqueConstraint(columnNames = "laatste_afspraak") })
@Audited
public class MammaUitnodiging extends Uitnodiging<MammaScreeningRonde>
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningRonde screeningRonde;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde standplaatsRonde;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaBrief brief;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "uitnodiging")
	private List<MammaAfspraak> afspraken = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfspraak laatsteAfspraak;

	@Column(nullable = false)
	private Boolean herinnered;

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal afstand;

	@Override
	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public MammaBrief getBrief()
	{
		return brief;
	}

	public void setBrief(MammaBrief brief)
	{
		this.brief = brief;
	}

	public List<MammaAfspraak> getAfspraken()
	{
		return afspraken;
	}

	public void setAfspraken(List<MammaAfspraak> afspraken)
	{
		this.afspraken = afspraken;
	}

	public MammaAfspraak getLaatsteAfspraak()
	{
		return laatsteAfspraak;
	}

	public void setLaatsteAfspraak(MammaAfspraak laatsteAfspraak)
	{
		this.laatsteAfspraak = laatsteAfspraak;
	}

	public Boolean getHerinnered()
	{
		return herinnered;
	}

	public void setHerinnered(Boolean herinnered)
	{
		this.herinnered = herinnered;
	}

	public BigDecimal getAfstand()
	{
		return afstand;
	}

	public void setAfstand(BigDecimal afstand)
	{
		this.afstand = afstand;
	}

	public MammaStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}
}
