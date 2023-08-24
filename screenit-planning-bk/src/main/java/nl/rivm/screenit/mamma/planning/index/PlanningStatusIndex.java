package nl.rivm.screenit.mamma.planning.index;

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

import java.time.LocalDateTime;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.util.EnvironmentUtil;

@Slf4j
public enum PlanningStatusIndex
{
	;

	private static final Integer MAX_WACHTTIJD_START_PLANNING_BLOKKING_POPUP = EnvironmentUtil.getIntegerEnvironmentVariable("MAX_WACHTTIJD_START_PLANNING_BLOKKING_POPUP", 60);

	private static MammaPlanningStatus status = MammaPlanningStatus.OPSTARTEN;

	private static Long soId = null;

	private static LocalDateTime startLangLopendeActie = LocalDateTime.now();

	public static MammaPlanningStatus get()
	{
		if (MammaPlanningStatus.OPSTARTEN != status && MammaPlanningStatus.ERROR != status && isNogNietLangGenoegNietOperationeel())
		{
			return MammaPlanningStatus.OPERATIONEEL;
		}
		return status;
	}

	private static boolean isNogNietLangGenoegNietOperationeel()
	{
		return status != MammaPlanningStatus.OPERATIONEEL && startLangLopendeActie.isAfter(LocalDateTime.now().minusSeconds(MAX_WACHTTIJD_START_PLANNING_BLOKKING_POPUP));
	}

	public static void set(MammaPlanningStatus status)
	{
		set(status, null);
	}

	public static void set(MammaPlanningStatus status, Long soId)
	{
		LOG.info("Status change to " + status);
		PlanningStatusIndex.status = status;
		PlanningStatusIndex.soId = soId;
		if (status != MammaPlanningStatus.OPERATIONEEL)
		{
			startLangLopendeActie = LocalDateTime.now();
		}
	}

	public static Long getSoId()
	{
		if (isNogNietLangGenoegNietOperationeel())
		{
			return null;
		}
		return soId;
	}
}
