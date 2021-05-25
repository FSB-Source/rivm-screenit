package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import nl.rivm.screenit.batch.jobs.cervix.CervixBaseLogListener;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixHpvOruBerichtenListener extends CervixBaseLogListener
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvOruBerichtenListener.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HPV_ORU_BERICHTEN_START;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HPV_ORU_BERICHTEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent event = super.eindLogging(jobExecution);

		ExecutionContext context = jobExecution.getExecutionContext();
		long verstuurd = context.getLong(CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD, 0);
		if (StringUtils.isBlank(event.getMelding()))
		{
			StringBuilder melding = new StringBuilder();
			melding.append("Er zijn ");
			melding.append(verstuurd);

			Optional.ofNullable((Map<Long, Integer>) jobExecution.getExecutionContext().get(CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB))
				.ifPresent(map -> {
					melding.append(" (");
					melding.append(map.entrySet().stream()
						.map(entry -> hibernateService.load(BMHKLaboratorium.class, entry.getKey()).getNaam() + ": " + entry.getValue())
						.collect(Collectors.joining(", ")));
					melding.append(")");
				});

			melding.append(" oru r01 bericht(en) verstuurd.");
			event.setMelding(melding.toString());
			LOG.info(melding.toString());
		}

		return event;
	}
}
