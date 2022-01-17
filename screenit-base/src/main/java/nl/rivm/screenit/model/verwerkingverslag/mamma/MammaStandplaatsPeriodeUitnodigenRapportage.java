package nl.rivm.screenit.model.verwerkingverslag.mamma;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "mamma", name = "standplaats_periode_uitnodigen_rapportage")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class MammaStandplaatsPeriodeUitnodigenRapportage extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsPeriode standplaatsPeriode;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date uitnodigenTotEnMet;

	@Column(nullable = false)
	private Long uitgenodigdAfspraak = 0L;

	@Column(nullable = false)
	private Long uitgenodigdOpen = 0L;

	@Column(nullable = false)
	private Long uitgenodigdMinderValide = 0L;

	@Column(nullable = false)
	private Long uitgenodigdSuspect = 0L;

	@Column(nullable = false)
	private Long uitgenodigdNaUitstel = 0L;

	@Column(nullable = false)
	private Long uitgesteldAchtervangUitstel = 0L;

	@Column(nullable = false)
	private Long uitgesteldMinderValideUitgewijktUitstel = 0L;

	public MammaStandplaatsRondeUitnodigenRapportage getStandplaatsRondeUitnodigenRapportage()
	{
		return standplaatsRondeUitnodigenRapportage;
	}

	public void setStandplaatsRondeUitnodigenRapportage(MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage)
	{
		this.standplaatsRondeUitnodigenRapportage = standplaatsRondeUitnodigenRapportage;
	}

	public MammaStandplaatsPeriode getStandplaatsPeriode()
	{
		return standplaatsPeriode;
	}

	public void setStandplaatsPeriode(MammaStandplaatsPeriode standplaatsPeriode)
	{
		this.standplaatsPeriode = standplaatsPeriode;
	}

	public Date getUitnodigenTotEnMet()
	{
		return uitnodigenTotEnMet;
	}

	public void setUitnodigenTotEnMet(Date uitnodigenTotEnMet)
	{
		this.uitnodigenTotEnMet = uitnodigenTotEnMet;
	}

	public Long getUitgenodigdAfspraak()
	{
		return uitgenodigdAfspraak;
	}

	public void setUitgenodigdAfspraak(Long uitgenodigdAfspraak)
	{
		this.uitgenodigdAfspraak = uitgenodigdAfspraak;
	}

	public Long getUitgenodigdOpen()
	{
		return uitgenodigdOpen;
	}

	public void setUitgenodigdOpen(Long uitgenodigdOpen)
	{
		this.uitgenodigdOpen = uitgenodigdOpen;
	}

	public Long getUitgenodigdMinderValide()
	{
		return uitgenodigdMinderValide;
	}

	public void setUitgenodigdMinderValide(Long uitgenodigdMinderValide)
	{
		this.uitgenodigdMinderValide = uitgenodigdMinderValide;
	}

	public Long getUitgenodigdSuspect()
	{
		return uitgenodigdSuspect;
	}

	public void setUitgenodigdSuspect(Long uitgenodigdSuspect)
	{
		this.uitgenodigdSuspect = uitgenodigdSuspect;
	}

	public Long getUitgenodigdNaUitstel()
	{
		return uitgenodigdNaUitstel;
	}

	public void setUitgenodigdNaUitstel(Long uitgenodigdNaUitstel)
	{
		this.uitgenodigdNaUitstel = uitgenodigdNaUitstel;
	}

	public Long getUitgesteldAchtervangUitstel()
	{
		return uitgesteldAchtervangUitstel;
	}

	public void setUitgesteldAchtervangUitstel(Long uitgesteldAchtervangUitstel)
	{
		this.uitgesteldAchtervangUitstel = uitgesteldAchtervangUitstel;
	}

	public Long getUitgesteldMinderValideUitgewijktUitstel()
	{
		return uitgesteldMinderValideUitgewijktUitstel;
	}

	public void setUitgesteldMinderValideUitgewijktUitstel(Long uitgesteldMinderValideUitgewijktUitstel)
	{
		this.uitgesteldMinderValideUitgewijktUitstel = uitgesteldMinderValideUitgewijktUitstel;
	}
}
