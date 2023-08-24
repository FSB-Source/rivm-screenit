package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "standplaats_ronde")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaStandplaatsRonde extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaStandplaats standplaats;

	@Column(nullable = true)
	private Integer afspraakDrempel;

	@OneToMany(mappedBy = "standplaatsRonde", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaStandplaatsPeriode> standplaatsPerioden = new ArrayList<>();

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S5)
	private BigDecimal interval;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats achtervangStandplaats;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats minderValideUitwijkStandplaats;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(
		schema = "mamma",
		name = "standplaats_ronde_afspraakcapaciteit_beschikbaar_voor",
		joinColumns = { @JoinColumn(name = "standplaats_ronde") },
		inverseJoinColumns = { @JoinColumn(name = "screeningorganisatie") })
	@DiffSpecs(displayProperty = "naam", listSupported = true)
	private List<ScreeningOrganisatie> afspraakcapaciteitBeschikbaarVoor = new ArrayList<>();

	@OneToOne(mappedBy = "standplaatsRonde", optional = true, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaKansberekeningStandplaatsRondeGemiddelden standplaatsRondeGemiddelden;

	@Column(nullable = false)
	private Boolean achtervangToegepast;

	@Temporal(TemporalType.DATE)
	private Date minderValideUitnodigenVanaf;

	@Column(nullable = false, precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S2)
	private BigDecimal extraMinderValideCapaciteitUitgenodigd = BigDecimal.ZERO;

	public MammaStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(MammaStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public List<MammaStandplaatsPeriode> getStandplaatsPerioden()
	{
		return standplaatsPerioden;
	}

	public void setStandplaatsPerioden(List<MammaStandplaatsPeriode> standplaatsPerioden)
	{
		this.standplaatsPerioden = standplaatsPerioden;
	}

	public BigDecimal getInterval()
	{
		return interval;
	}

	public void setInterval(BigDecimal interval)
	{
		this.interval = interval;
	}

	public Integer getAfspraakDrempel()
	{
		return afspraakDrempel;
	}

	public void setAfspraakDrempel(Integer afspraakDrempel)
	{
		this.afspraakDrempel = afspraakDrempel;
	}

	public MammaStandplaats getAchtervangStandplaats()
	{
		return achtervangStandplaats;
	}

	public void setAchtervangStandplaats(MammaStandplaats achtervangStandplaats)
	{
		this.achtervangStandplaats = achtervangStandplaats;
	}

	public MammaStandplaats getMinderValideUitwijkStandplaats()
	{
		return minderValideUitwijkStandplaats;
	}

	public void setMinderValideUitwijkStandplaats(MammaStandplaats minderValideUitwijkStandplaats)
	{
		this.minderValideUitwijkStandplaats = minderValideUitwijkStandplaats;
	}

	public MammaKansberekeningStandplaatsRondeGemiddelden getStandplaatsRondeGemiddelden()
	{
		return standplaatsRondeGemiddelden;
	}

	public void setStandplaatsRondeGemiddelden(MammaKansberekeningStandplaatsRondeGemiddelden standplaatsRondeGemiddelden)
	{
		this.standplaatsRondeGemiddelden = standplaatsRondeGemiddelden;
	}

	public Boolean getAchtervangToegepast()
	{
		return achtervangToegepast;
	}

	public void setAchtervangToegepast(Boolean achtervangToegepast)
	{
		this.achtervangToegepast = achtervangToegepast;
	}

	public Date getMinderValideUitnodigenVanaf()
	{
		return minderValideUitnodigenVanaf;
	}

	public void setMinderValideUitnodigenVanaf(Date minderValideUitnodigenVanaf)
	{
		this.minderValideUitnodigenVanaf = minderValideUitnodigenVanaf;
	}

	public BigDecimal getExtraMinderValideCapaciteitUitgenodigd()
	{
		return extraMinderValideCapaciteitUitgenodigd;
	}

	public void setExtraMinderValideCapaciteitUitgenodigd(BigDecimal extraMinderValideCapaciteitUitgenodigd)
	{
		this.extraMinderValideCapaciteitUitgenodigd = extraMinderValideCapaciteitUitgenodigd;
	}

	public List<ScreeningOrganisatie> getAfspraakcapaciteitBeschikbaarVoor()
	{
		return afspraakcapaciteitBeschikbaarVoor;
	}

	public void setAfspraakcapaciteitBeschikbaarVoor(List<ScreeningOrganisatie> afspraakcapaciteitBeschikbaarVoor)
	{
		this.afspraakcapaciteitBeschikbaarVoor = afspraakcapaciteitBeschikbaarVoor;
	}
}
