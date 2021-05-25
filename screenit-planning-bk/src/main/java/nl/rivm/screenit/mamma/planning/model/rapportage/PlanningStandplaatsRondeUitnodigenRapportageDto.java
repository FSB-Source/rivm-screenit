package nl.rivm.screenit.mamma.planning.model.rapportage;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;

import java.util.ArrayList;
import java.util.List;

public class PlanningStandplaatsRondeUitnodigenRapportageDto
{
	private Long standplaatsRondeId;

	private final List<PlanningStandplaatsPeriodeUitnodigenRapportageDto> standplaatsPeriodeUitnodigenRapportages = new ArrayList<>();

	private Long totaalTotaal = -1L;

	private Long totaalVervolgRonde = -1L;

	private Long totaalEersteRonde = -1L;

	private Long totaalDubbeleTijd = -1L;

	private Long totaalMinderValide = -1L;

	private Long totaalTehuis = -1L;

	private Long totaalSuspect = -1L;

	private Long uitTeNodigenTotaal = -1L;

	private Long uitTeNodigenVervolgRonde = -1L;

	private Long uitTeNodigenEersteRonde = -1L;

	private Long uitTeNodigenDubbeleTijd = -1L;

	private Long uitTeNodigenMinderValide = -1L;

	private Long uitTeNodigenTehuis = -1L;

	private Long uitTeNodigenSuspect = -1L;

	private MammaStandplaatsRondeRapportageStatus status;

	public Long getStandplaatsRondeId()
	{
		return standplaatsRondeId;
	}

	public void setStandplaatsRondeId(Long standplaatsRondeId)
	{
		this.standplaatsRondeId = standplaatsRondeId;
	}

	public List<PlanningStandplaatsPeriodeUitnodigenRapportageDto> getStandplaatsPeriodeUitnodigenRapportages()
	{
		return standplaatsPeriodeUitnodigenRapportages;
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
