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

import java.time.LocalDate;

import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;

public class PlanningBlokkade extends PlanningEntiteit
{
	private MammaBlokkadeType blokkadeType;

	private PlanningStandplaats standplaats;

	private PlanningScreeningsEenheid screeningsEenheid;

	private PlanningScreeningsOrganisatie screeningsOrganisatie;

	private LocalDate vanaf;

	private LocalDate totEnMet;

	public PlanningBlokkade(Long id, MammaBlokkadeType blokkadeType, PlanningStandplaats standplaats,
		PlanningScreeningsEenheid screeningsEenheid, PlanningScreeningsOrganisatie screeningsOrganisatie, LocalDate vanaf, LocalDate totEnMet)
	{
		super(id);
		this.blokkadeType = blokkadeType;
		this.standplaats = standplaats;
		this.screeningsEenheid = screeningsEenheid;
		this.screeningsOrganisatie = screeningsOrganisatie;
		this.vanaf = vanaf;
		this.totEnMet = totEnMet;
	}

	public MammaBlokkadeType getBlokkadeType()
	{
		return blokkadeType;
	}

	public void setBlokkadeType(MammaBlokkadeType blokkadeType)
	{
		this.blokkadeType = blokkadeType;
	}

	public LocalDate getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(LocalDate vanaf)
	{
		this.vanaf = vanaf;
	}

	public LocalDate getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(LocalDate totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public PlanningStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(PlanningStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public PlanningScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(PlanningScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public PlanningScreeningsOrganisatie getScreeningsOrganisatie()
	{
		return screeningsOrganisatie;
	}

	public void setScreeningsOrganisatie(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		this.screeningsOrganisatie = screeningsOrganisatie;
	}
}
