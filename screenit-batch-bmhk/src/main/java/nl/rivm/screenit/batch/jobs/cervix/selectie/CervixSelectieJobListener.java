package nl.rivm.screenit.batch.jobs.cervix.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.CervixSelectieBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixSelectieRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixSelectieJobListener extends BaseLogListener
{
	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_SELECTIE_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_SELECTIE_BEEINDIGD;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixSelectieBeeindigdLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();
		long aantal = context.getLong(CervixSelectieConstants.SELECTIE_AANTAL_KEY, 0);
		long aantalVooraankondiging = context.getLong(CervixSelectieConstants.VOORAANKONDIGING_SELECTIE_AANTAL_KEY, 0);

		var rapportage = new CervixSelectieRapportage();
		rapportage.setDatumVerwerking(dateSupplier.getDate());
		rapportage.setAantal(aantal);

		var selectieBeeindigdLogEvent = (CervixSelectieBeeindigdLogEvent) super.eindLogging(jobExecution);
		selectieBeeindigdLogEvent.setRapportage(rapportage);
		selectieBeeindigdLogEvent.setMelding("Er zijn " + aantal + " client(en) uitgenodigd en " + aantalVooraankondiging + " vooraankondiging(en) gemaakt.");

		hibernateService.saveOrUpdate(rapportage);
		hibernateService.saveOrUpdate(selectieBeeindigdLogEvent);
		return selectieBeeindigdLogEvent;
	}
}
