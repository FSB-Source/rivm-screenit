package nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.BriefType;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
public class BezwaarBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{
	public static final String KEY_SCREENINGORGANISATIEID = "screeningorganisatie.id";

	public static final String KEY_BRIEFTYPE = "brieftype";

	@Override
	protected void fillingData(Map<String, ExecutionContext> map, ScreeningOrganisatie organisatie)
	{
		for (BriefType briefType : getBriefTypes())
		{
			if (briefType.getVerzendendeOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE)
			{
				ExecutionContext executionContext = new ExecutionContext();
				executionContext.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
				executionContext.put(KEY_BRIEFTYPE, briefType.name());
				map.put(organisatie.getId() + briefType.name(), executionContext);
			}
		}
	}

	private List<BriefType> getBriefTypes()
	{
		return BriefType.CLIENT_BEZWAAR_BRIEVEN;
	}
}
