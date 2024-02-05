package nl.rivm.screenit.mamma.planning.dao.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.util.DateUtil;

@Getter
@Setter
public class StandplaatsPeriodeDaoDto
{
	private long standplaatsPeriodeId;

	private long standplaatsRondeId;

	private long regioId;

	private int wekenVanTevorenUitnodigen;

	@Setter(AccessLevel.NONE)
	private LocalDate vanaf;

	@Setter(AccessLevel.NONE)
	private LocalDate totEnMet;

	public Date getVanafUtilDate()
	{
		return DateUtil.toUtilDate(vanaf);
	}

	public void setVanafUtilDate(Date vanafUtilDate)
	{
		vanaf = DateUtil.toLocalDate(vanafUtilDate);
	}

	public Date getTotEnMetUtilDate()
	{
		return DateUtil.toUtilDate(vanaf);
	}

	public void setTotEnMetUtilDate(Date totEnMetUtilDate)
	{
		totEnMet = DateUtil.toLocalDate(totEnMetUtilDate);
	}

}
