package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.CervixBaseLogListener;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB;

@Component
@Slf4j
@AllArgsConstructor
public class CervixHpvOruBerichtenListener extends CervixBaseLogListener
{

	private final HibernateService hibernateService;

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
		var event = super.eindLogging(jobExecution);

		var context = jobExecution.getExecutionContext();
		long verstuurd = context.getLong(CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURD, 0);
		if (StringUtils.isBlank(event.getMelding()))
		{
			var melding = new StringBuilder();
			melding.append("Er zijn ");
			melding.append(verstuurd);

			Optional.ofNullable((Map<Long, Integer>) jobExecution.getExecutionContext().get(CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB))
				.ifPresent(map ->
				{
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
