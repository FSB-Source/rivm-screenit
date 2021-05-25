package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.mamma.MammaMammografie;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaBeeldenStatusSignalerenWriter extends BaseWriter<MammaMammografie>
{
	private final Logger LOG = LoggerFactory.getLogger(MammaBeeldenStatusSignalerenWriter.class);

	@Override
	protected void write(MammaMammografie mammografie)
	{
		long uitnodigingsNr = mammografie.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingsNr();
		Date statusDatum = mammografie.getIlmStatusDatum();
		LOG.info("Gesignaleerd, uitnodigingsNr: {}, statusDatum: {}", uitnodigingsNr, statusDatum);

		Map<Long, Date> map = (Map<Long, Date>) getJobExecution().getExecutionContext().get(MammaIlmJobListener.KEY_BEELDEN_STATUS_ENTRIES);
		if (map == null)
		{
			map = new HashMap<>();
			getExecutionContext().put(MammaIlmJobListener.KEY_BEELDEN_STATUS_ENTRIES, map);
		}
		map.put(uitnodigingsNr, statusDatum);
	}
}
