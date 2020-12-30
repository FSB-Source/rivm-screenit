package nl.rivm.screenit.batch.jobs.cervix.vervolgonderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.logging.CervixVervolgonderzoekBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixVervolgonderzoekRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixVervolgonderzoekJobListener extends BaseLogListener
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

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
		return LogGebeurtenis.CERVIX_VERVOLGONDERZOEK_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_VERVOLGONDERZOEK_BEEINDIGD;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixVervolgonderzoekBeeindigdLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();
		long aantal = context.getLong(CervixVervolgonderzoekConstants.VERVOLGONDERZOEK_AANTAL_KEY, 0);

		CervixVervolgonderzoekRapportage rapportage = new CervixVervolgonderzoekRapportage();
		rapportage.setDatumVerwerking(dateSupplier.getDate());
		rapportage.setAantal(aantal);

		CervixVervolgonderzoekBeeindigdLogEvent vervolgonderzoekBeeindigdLogEvent = (CervixVervolgonderzoekBeeindigdLogEvent) super.eindLogging(jobExecution);
		vervolgonderzoekBeeindigdLogEvent.setRapportage(rapportage);
		vervolgonderzoekBeeindigdLogEvent.setMelding("Er zijn " + aantal + " client(en) uitgenodigd voor een controle-uitstrijkje");

		hibernateService.saveOrUpdate(rapportage);
		hibernateService.saveOrUpdate(vervolgonderzoekBeeindigdLogEvent);
		return vervolgonderzoekBeeindigdLogEvent;
	}
}
