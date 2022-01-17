package nl.rivm.screenit.model.cervix.facturatie;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class CervixLabTarief extends CervixTarief
{

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal hpvAnalyseUitstrijkjeTarief;

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal hpvAnalyseZasTarief;

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal cytologieNaHpvUitstrijkjeTarief;

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal cytologieNaHpvZasTarief;

	@Column(nullable = true, precision = HibernateMagicNumber.P5, scale = HibernateMagicNumber.S2)
	private BigDecimal cytologieVervolguitstrijkjeTarief;

	@ManyToOne(fetch = FetchType.LAZY)
	private BMHKLaboratorium bmhkLaboratorium;

	public BigDecimal getHpvAnalyseUitstrijkjeTarief()
	{
		return hpvAnalyseUitstrijkjeTarief;
	}

	public void setHpvAnalyseUitstrijkjeTarief(BigDecimal hpvAnalyseUitstrijkjeTarief)
	{
		this.hpvAnalyseUitstrijkjeTarief = hpvAnalyseUitstrijkjeTarief;
	}

	public BigDecimal getHpvAnalyseZasTarief()
	{
		return hpvAnalyseZasTarief;
	}

	public void setHpvAnalyseZasTarief(BigDecimal hpvAnalyseZasTarief)
	{
		this.hpvAnalyseZasTarief = hpvAnalyseZasTarief;
	}

	public BigDecimal getCytologieNaHpvUitstrijkjeTarief()
	{
		return cytologieNaHpvUitstrijkjeTarief;
	}

	public void setCytologieNaHpvUitstrijkjeTarief(BigDecimal cytologieNaHpvUitstrijkjeTarief)
	{
		this.cytologieNaHpvUitstrijkjeTarief = cytologieNaHpvUitstrijkjeTarief;
	}

	public BigDecimal getCytologieNaHpvZasTarief()
	{
		return cytologieNaHpvZasTarief;
	}

	public void setCytologieNaHpvZasTarief(BigDecimal cytologieNaHpvZasTarief)
	{
		this.cytologieNaHpvZasTarief = cytologieNaHpvZasTarief;
	}

	public BigDecimal getCytologieVervolguitstrijkjeTarief()
	{
		return cytologieVervolguitstrijkjeTarief;
	}

	public void setCytologieVervolguitstrijkjeTarief(BigDecimal cytologieVervolguitstrijkjeTarief)
	{
		this.cytologieVervolguitstrijkjeTarief = cytologieVervolguitstrijkjeTarief;
	}

	public BMHKLaboratorium getBmhkLaboratorium()
	{
		return bmhkLaboratorium;
	}

	public void setBmhkLaboratorium(BMHKLaboratorium bmhkLaboratorium)
	{
		this.bmhkLaboratorium = bmhkLaboratorium;
	}

}
