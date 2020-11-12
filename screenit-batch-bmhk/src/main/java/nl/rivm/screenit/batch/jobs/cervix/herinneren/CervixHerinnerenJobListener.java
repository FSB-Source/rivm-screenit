package nl.rivm.screenit.batch.jobs.cervix.herinneren;

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
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.CervixHerinnerenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHerinnerenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHerinnerenRapportageBriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixHerinnerenJobListener extends BaseLogListener
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
		return LogGebeurtenis.CERVIX_HERINNEREN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HERINNEREN_BEEINDIGD;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixHerinnerenBeeindigdLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();

		long totaalAantal = context.getLong(CervixHerinnerenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, 0);

		CervixHerinnerenRapportage rapportage = new CervixHerinnerenRapportage();
		rapportage.setDatumVerwerking(dateSupplier.getDate());
		rapportage.setTotaalAantal(totaalAantal);

		aantallenContextVerwerken(CervixHerinnerenConstants.BRIEF_TYPE_KEY, new AantalVerwerker<BriefType>()
		{
			@Override
			protected void verwerk(BriefType briefType, long aantal)
			{
				rapportage.getBriefTypen().add(new CervixHerinnerenRapportageBriefType(rapportage, briefType, aantal));
			}
		});

		CervixHerinnerenBeeindigdLogEvent herinneringBeeindigdLogEvent = (CervixHerinnerenBeeindigdLogEvent) super.eindLogging(jobExecution);
		herinneringBeeindigdLogEvent.setRapportage(rapportage);
		herinneringBeeindigdLogEvent.setMelding("Er zijn " + totaalAantal + " herinneringen verstuurd");

		hibernateService.saveOrUpdate(rapportage);
		hibernateService.saveOrUpdate(herinneringBeeindigdLogEvent);
		return herinneringBeeindigdLogEvent;
	}
}
