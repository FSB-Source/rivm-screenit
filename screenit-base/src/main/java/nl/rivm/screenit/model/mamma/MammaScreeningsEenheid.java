package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaDuurMinderValideAfspraak;
import nl.rivm.screenit.util.DiffSpecs;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "screenings_eenheid")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
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

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public String getCode()
	{
		return code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public String getIpAdres()
	{
		return ipAdres;
	}

	public void setIpAdres(String ipAdres)
	{
		this.ipAdres = ipAdres;
	}

	public Boolean getIsMobiel()
	{
		return isMobiel;
	}

	public void setIsMobiel(Boolean isMobiel)
	{
		this.isMobiel = isMobiel;
	}

	public Boolean getHeeftLift()
	{
		return heeftLift;
	}

	public void setHeeftLift(Boolean heeftLift)
	{
		this.heeftLift = heeftLift;
	}

	@Override
	public Boolean getActief()
	{
		return this.actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public List<MammaBlokkade> getBlokkades()
	{
		return blokkades;
	}

	public void setBlokkades(List<MammaBlokkade> blokkades)
	{
		this.blokkades = blokkades;
	}

	public List<MammaStandplaatsPeriode> getStandplaatsPerioden()
	{
		return standplaatsPerioden;
	}

	public void setStandplaatsPerioden(List<MammaStandplaatsPeriode> standplaatsPerioden)
	{
		this.standplaatsPerioden = standplaatsPerioden;
	}

	public Date getUitgenodigdTotEnMet()
	{
		return uitgenodigdTotEnMet;
	}

	public void setUitgenodigdTotEnMet(Date uitgenodigdTotEnMet)
	{
		this.uitgenodigdTotEnMet = uitgenodigdTotEnMet;
	}

	public Date getUitnodigenTotEnMet()
	{
		return uitnodigenTotEnMet;
	}

	public void setUitnodigenTotEnMet(Date uitnodigenTotEnMet)
	{
		this.uitnodigenTotEnMet = uitnodigenTotEnMet;
	}

	public Date getVrijgegevenTotEnMet()
	{
		return vrijgegevenTotEnMet;
	}

	public void setVrijgegevenTotEnMet(Date vrijgegevenTotEnMet)
	{
		this.vrijgegevenTotEnMet = vrijgegevenTotEnMet;
	}

	public BeoordelingsEenheid getBeoordelingsEenheid()
	{
		return beoordelingsEenheid;
	}

	public void setBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		this.beoordelingsEenheid = beoordelingsEenheid;
	}

	public BigDecimal getInterval()
	{
		return interval;
	}

	public void setInterval(BigDecimal interval)
	{
		this.interval = interval;
	}

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

	public Date getHerhalingsWeek()
	{
		return herhalingsWeek;
	}

	public void setHerhalingsWeek(Date herhalingsWeek)
	{
		this.herhalingsWeek = herhalingsWeek;
	}

	public List<MammaMammograaf> getMammografen()
	{
		return mammografen;
	}

	public void setMammografen(List<MammaMammograaf> mammografen)
	{
		this.mammografen = mammografen;
	}

	public BeoordelingsEenheid getTijdelijkeBeoordelingsEenheid()
	{
		return tijdelijkeBeoordelingsEenheid;
	}

	public void setTijdelijkeBeoordelingsEenheid(BeoordelingsEenheid tijdelijkeBeoordelingsEenheid)
	{
		this.tijdelijkeBeoordelingsEenheid = tijdelijkeBeoordelingsEenheid;
	}

	public Date getTijdelijkeBeVanafDatum()
	{
		return tijdelijkeBeVanafDatum;
	}

	public void setTijdelijkeBeVanafDatum(Date tijdelijkeBeVanafDatum)
	{
		this.tijdelijkeBeVanafDatum = tijdelijkeBeVanafDatum;
	}

	public Date getTijdelijkeBeTotEnMetDatum()
	{
		return tijdelijkeBeTotEnMetDatum;
	}

	public void setTijdelijkeBeTotEnMetDatum(Date tijdelijkeBeTotEnMetDatum)
	{
		this.tijdelijkeBeTotEnMetDatum = tijdelijkeBeTotEnMetDatum;
	}

	public MammaDuurMinderValideAfspraak getDuurMinderValideAfspraak()
	{
		return duurMinderValideAfspraak;
	}

	public void setDuurMinderValideAfspraak(MammaDuurMinderValideAfspraak duurMinderValideAfspraak)
	{
		this.duurMinderValideAfspraak = duurMinderValideAfspraak;
	}

	public Date getMinderValidePeriode1Vanaf()
	{
		return minderValidePeriode1Vanaf;
	}

	public void setMinderValidePeriode1Vanaf(Date minderValidePeriode1Vanaf)
	{
		this.minderValidePeriode1Vanaf = minderValidePeriode1Vanaf;
	}

	public Date getMinderValidePeriode1TotEnMet()
	{
		return minderValidePeriode1TotEnMet;
	}

	public void setMinderValidePeriode1TotEnMet(Date minderValidePeriode1TotEnMet)
	{
		this.minderValidePeriode1TotEnMet = minderValidePeriode1TotEnMet;
	}

	public Date getMinderValidePeriode2Vanaf()
	{
		return minderValidePeriode2Vanaf;
	}

	public void setMinderValidePeriode2Vanaf(Date minderValidePeriode2Vanaf)
	{
		this.minderValidePeriode2Vanaf = minderValidePeriode2Vanaf;
	}

	public Date getMinderValidePeriode2TotEnMet()
	{
		return minderValidePeriode2TotEnMet;
	}

	public void setMinderValidePeriode2TotEnMet(Date minderValidePeriode2TotEnMet)
	{
		this.minderValidePeriode2TotEnMet = minderValidePeriode2TotEnMet;
	}
}
