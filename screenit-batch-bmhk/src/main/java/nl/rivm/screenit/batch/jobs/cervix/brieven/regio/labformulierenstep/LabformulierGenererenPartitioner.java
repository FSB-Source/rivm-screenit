package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep;

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

import java.util.Map;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.model.ScreeningOrganisatie;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
public class LabformulierGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{

	public static final String KEY_SCREENINGORGANISATIEID = "screeningorganisatie.id";

	@Override
	protected void fillingData(Map<String, ExecutionContext> map, ScreeningOrganisatie organisatie)
	{
		var executionContext = new ExecutionContext();
		executionContext.putLong(KEY_SCREENINGORGANISATIEID, organisatie.getId());
		map.put("ScreeningOrganisatie_" + organisatie.getId().toString(), executionContext);
	}
}
