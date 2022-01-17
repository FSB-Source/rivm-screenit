package nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

public class ProjectCounterHolder
{
	private Long projectGroepId;

	private Long aantalVerstuurd;

	public ProjectCounterHolder(Long projectGroepId, Long aantalVerstuurd)
	{
		this.projectGroepId = projectGroepId;
		this.aantalVerstuurd = aantalVerstuurd;
	}

	public Long getProjectGroepId()
	{
		return projectGroepId;
	}

	public void setProjectGroepId(Long projectGroepId)
	{
		this.projectGroepId = projectGroepId;
	}

	public Long getAantalVerstuurd()
	{
		return aantalVerstuurd;
	}

	public void setAantalVerstuurd(Long aantalVerstuurd)
	{
		this.aantalVerstuurd = aantalVerstuurd;
	}
}
