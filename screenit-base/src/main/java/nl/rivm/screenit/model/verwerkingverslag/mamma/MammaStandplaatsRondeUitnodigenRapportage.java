package nl.rivm.screenit.model.verwerkingverslag.mamma;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "mamma", name = "standplaats_ronde_uitnodigen_rapportage")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class MammaStandplaatsRondeUitnodigenRapportage extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaUitnodigenRapportage uitnodigenRapportage;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde standplaatsRonde;

	@OneToMany(mappedBy = "standplaatsRondeUitnodigenRapportage", fetch = FetchType.LAZY)
	@Cascade(CascadeType.DELETE)
	private List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages = new ArrayList<>();

	@Column(nullable = false)
	private Long totaalTotaal;

	@Column(nullable = false)
	private Long totaalVervolgRonde;

	@Column(nullable = false)
	private Long totaalEersteRonde;

	@Column(nullable = false)
	private Long totaalDubbeleTijd;

	@Column(nullable = false)
	private Long totaalMinderValide;

	@Column(nullable = false)
	private Long totaalTehuis;

	@Column(nullable = false)
	private Long totaalSuspect;

	@Column(nullable = false)
	private Long uitTeNodigenTotaal;

	@Column(nullable = false)
	private Long uitTeNodigenVervolgRonde;

	@Column(nullable = false)
	private Long uitTeNodigenEersteRonde;

	@Column(nullable = false)
	private Long uitTeNodigenDubbeleTijd;

	@Column(nullable = false)
	private Long uitTeNodigenMinderValide;

	@Column(nullable = false)
	private Long uitTeNodigenTehuis;

	@Column(nullable = false)
	private Long uitTeNodigenSuspect;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaStandplaatsRondeRapportageStatus status;

	public MammaUitnodigenRapportage getUitnodigenRapportage()
	{
		return uitnodigenRapportage;
	}

	public void setUitnodigenRapportage(MammaUitnodigenRapportage uitnodigenRapportage)
	{
		this.uitnodigenRapportage = uitnodigenRapportage;
	}

	public MammaStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}

	public List<MammaStandplaatsPeriodeUitnodigenRapportage> getStandplaatsPeriodeUitnodigenRapportages()
	{
		return standplaatsPeriodeUitnodigenRapportages;
	}

	public void setStandplaatsPeriodeUitnodigenRapportages(
		List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages)
	{
		this.standplaatsPeriodeUitnodigenRapportages = standplaatsPeriodeUitnodigenRapportages;
	}

	public Long getTotaalTotaal()
	{
		return totaalTotaal;
	}

	public void setTotaalTotaal(Long totaalTotaal)
	{
		this.totaalTotaal = totaalTotaal;
	}

	public Long getTotaalVervolgRonde()
	{
		return totaalVervolgRonde;
	}

	public void setTotaalVervolgRonde(Long totaalVervolgRonde)
	{
		this.totaalVervolgRonde = totaalVervolgRonde;
	}

	public Long getTotaalEersteRonde()
	{
		return totaalEersteRonde;
	}

	public void setTotaalEersteRonde(Long totaalEersteRonde)
	{
		this.totaalEersteRonde = totaalEersteRonde;
	}

	public Long getTotaalDubbeleTijd()
	{
		return totaalDubbeleTijd;
	}

	public void setTotaalDubbeleTijd(Long totaalDubbeleTijd)
	{
		this.totaalDubbeleTijd = totaalDubbeleTijd;
	}

	public Long getTotaalMinderValide()
	{
		return totaalMinderValide;
	}

	public void setTotaalMinderValide(Long totaalMinderValide)
	{
		this.totaalMinderValide = totaalMinderValide;
	}

	public Long getTotaalTehuis()
	{
		return totaalTehuis;
	}

	public void setTotaalTehuis(Long totaalTehuis)
	{
		this.totaalTehuis = totaalTehuis;
	}

	public Long getTotaalSuspect()
	{
		return totaalSuspect;
	}

	public void setTotaalSuspect(Long totaalSuspect)
	{
		this.totaalSuspect = totaalSuspect;
	}

	public Long getUitTeNodigenTotaal()
	{
		return uitTeNodigenTotaal;
	}

	public void setUitTeNodigenTotaal(Long uitTeNodigenTotaal)
	{
		this.uitTeNodigenTotaal = uitTeNodigenTotaal;
	}

	public Long getUitTeNodigenVervolgRonde()
	{
		return uitTeNodigenVervolgRonde;
	}

	public void setUitTeNodigenVervolgRonde(Long uitTeNodigenVervolgRonde)
	{
		this.uitTeNodigenVervolgRonde = uitTeNodigenVervolgRonde;
	}

	public Long getUitTeNodigenEersteRonde()
	{
		return uitTeNodigenEersteRonde;
	}

	public void setUitTeNodigenEersteRonde(Long uitTeNodigenEersteRonde)
	{
		this.uitTeNodigenEersteRonde = uitTeNodigenEersteRonde;
	}

	public Long getUitTeNodigenDubbeleTijd()
	{
		return uitTeNodigenDubbeleTijd;
	}

	public void setUitTeNodigenDubbeleTijd(Long uitTeNodigenDubbeleTijd)
	{
		this.uitTeNodigenDubbeleTijd = uitTeNodigenDubbeleTijd;
	}

	public Long getUitTeNodigenMinderValide()
	{
		return uitTeNodigenMinderValide;
	}

	public void setUitTeNodigenMinderValide(Long uitTeNodigenMinderValide)
	{
		this.uitTeNodigenMinderValide = uitTeNodigenMinderValide;
	}

	public Long getUitTeNodigenTehuis()
	{
		return uitTeNodigenTehuis;
	}

	public void setUitTeNodigenTehuis(Long uitTeNodigenTehuis)
	{
		this.uitTeNodigenTehuis = uitTeNodigenTehuis;
	}

	public Long getUitTeNodigenSuspect()
	{
		return uitTeNodigenSuspect;
	}

	public void setUitTeNodigenSuspect(Long uitTeNodigenSuspect)
	{
		this.uitTeNodigenSuspect = uitTeNodigenSuspect;
	}

	public MammaStandplaatsRondeRapportageStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaStandplaatsRondeRapportageStatus status)
	{
		this.status = status;
	}
}
