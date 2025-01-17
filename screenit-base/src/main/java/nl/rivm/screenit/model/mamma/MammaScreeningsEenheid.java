package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaDuurMinderValideAfspraak;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "screenings_eenheid")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Getter
@Setter
public class MammaScreeningsEenheid extends AbstractHibernateObject implements IActief
{
	private static final long serialVersionUID = 1L;

	@Column(length = HibernateMagicNumber.L255)
	private String naam;

	@Column(nullable = false, unique = true, length = HibernateMagicNumber.L255)
	private String code;

	@Column(nullable = false, unique = true)
	private String ipAdres;

	@Column(nullable = false)
	private Boolean isMobiel;

	@Column(nullable = false)
	private Boolean heeftLift;

	@Column(nullable = false)
	private Boolean tomosyntheseMogelijk;

	@Column(nullable = false)
	private Boolean actief;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private BeoordelingsEenheid beoordelingsEenheid;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private BeoordelingsEenheid tijdelijkeBeoordelingsEenheid;

	@Temporal(TemporalType.DATE)
	@Column(nullable = true)
	private Date tijdelijkeBeVanafDatum;

	@Temporal(TemporalType.DATE)
	@Column(nullable = true)
	private Date tijdelijkeBeTotEnMetDatum;

	@OneToMany(mappedBy = "screeningsEenheid", fetch = FetchType.LAZY)
	private List<MammaBlokkade> blokkades = new ArrayList<>();

	@OneToMany(mappedBy = "screeningsEenheid", fetch = FetchType.LAZY)
	private List<MammaStandplaatsPeriode> standplaatsPerioden = new ArrayList<>();

	@OneToMany(mappedBy = "screeningsEenheid", fetch = FetchType.LAZY)
	private List<MammaMammograaf> mammografen = new ArrayList<>();

	@Temporal(TemporalType.DATE)
	private Date uitgenodigdTotEnMet;

	@Temporal(TemporalType.DATE)
	private Date uitnodigenTotEnMet;

	@Temporal(TemporalType.DATE)
	private Date vrijgegevenTotEnMet;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date herhalingsWeek;

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S5)
	private BigDecimal interval;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaDuurMinderValideAfspraak duurMinderValideAfspraak;

	@Transient
	private PlanningScreeningsEenheidMetaDataDto metaDataDto;

	@Column
	@Temporal(TemporalType.TIME)
	private Date minderValidePeriode1Vanaf;

	@Column
	@Temporal(TemporalType.TIME)
	private Date minderValidePeriode1TotEnMet;

	@Column
	@Temporal(TemporalType.TIME)
	private Date minderValidePeriode2Vanaf;

	@Column
	@Temporal(TemporalType.TIME)
	private Date minderValidePeriode2TotEnMet;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "screeningsEenheid")
	@Getter
	@Setter
	@NotAudited
	@SkipFieldForDiff
	private MammaScreeningsEenheidStatus status;

	@Transient
	public PlanningScreeningsEenheidMetaDataDto getMetaDataDto()
	{
		return metaDataDto;
	}

	@Transient
	public void setMetaDataDto(PlanningScreeningsEenheidMetaDataDto metaDataDto)
	{
		this.metaDataDto = metaDataDto;
	}
}
