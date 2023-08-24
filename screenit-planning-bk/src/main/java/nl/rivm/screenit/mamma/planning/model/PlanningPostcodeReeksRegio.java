package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;

public final class PlanningPostcodeReeksRegio
{
	private final String cijfer;

	private final Set<PlanningClient> clientSet = new HashSet<>();

	private final PlanningBenodigd benodigd = new PlanningBenodigd();

	private LocalDate uitnodigingStreefDatum;

	public PlanningPostcodeReeksRegio(String cijfer)
	{
		this.cijfer = cijfer;
	}

	public String getCijfer()
	{
		return cijfer;
	}

	public PlanningBenodigd getBenodigd()
	{
		return benodigd;
	}

	public Set<PlanningClient> getClientSet()
	{
		return clientSet;
	}

	public LocalDate getUitnodigingStreefDatum()
	{
		return uitnodigingStreefDatum;
	}

	public void setUitnodigingStreefDatum(LocalDate uitnodigingStreefDatum)
	{
		this.uitnodigingStreefDatum = uitnodigingStreefDatum;
	}
}
