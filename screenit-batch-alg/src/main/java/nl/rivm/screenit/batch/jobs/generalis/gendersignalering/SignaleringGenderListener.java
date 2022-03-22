package nl.rivm.screenit.batch.jobs.generalis.gendersignalering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class SignaleringGenderListener extends BaseLogListener
{
	static final String TOTAAL_AANTAL_BRIEVEN_KEY = "gendersignalering.totaal.aantal.brieven";

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
		return LogGebeurtenis.SIGNALERING_GENDER_BATCH_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.SIGNALERING_GENDER_BATCH_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();

		long totaalAantal = context.getLong(TOTAAL_AANTAL_BRIEVEN_KEY, 0);

		LogEvent logEvent = super.eindLogging(jobExecution);
		logEvent.setMelding("Er zijn " + totaalAantal + " brieven aangemaakt");

		hibernateService.saveOrUpdate(logEvent);
		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

}
