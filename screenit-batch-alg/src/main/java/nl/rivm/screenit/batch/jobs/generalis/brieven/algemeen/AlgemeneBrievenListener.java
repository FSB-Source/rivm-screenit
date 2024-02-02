package nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.HashMap;

import nl.rivm.screenit.batch.jobs.brieven.AbstractBrievenGenererenListener;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
public class AlgemeneBrievenListener extends AbstractBrievenGenererenListener
{
	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var map = new HashMap<Long, Integer>();
		map.put(0L, 0);
		jobExecution.getExecutionContext().put(getRapportageAantalBrievenKey(), map);
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return AlgemeneBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.ALGEMENE_BRIEVEN_BATCH_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.ALGEMENE_BRIEVEN_BATCH_AFGEROND;
	}

}
