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

import java.util.Date;

public class PlanningStandplaatsPeriodeUitnodigenRapportageDto
{

	private Long standplaatsPeriodeId;

	private Date uitnodigenTotEnMet;

	private Long uitgenodigdAfspraak = 0L;

	private Long uitgenodigdOpen = 0L;

	private Long uitgenodigdMinderValide = 0L;

	private Long uitgenodigdSuspect = 0L;

	private Long uitgenodigdNaUitstel = 0L;

	private Long uitgesteldAchtervangUitstel = 0L;

	private Long uitgesteldMinderValideUitgewijktUitstel = 0L;

	public Long getStandplaatsPeriodeId()
	{
		return standplaatsPeriodeId;
	}

	public void setStandplaatsPeriodeId(Long standplaatsPeriodeId)
	{
		this.standplaatsPeriodeId = standplaatsPeriodeId;
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
