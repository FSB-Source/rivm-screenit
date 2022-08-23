package nl.rivm.screenit.batch.jobs.mamma.uitnodigen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaUitnodigenLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIntervalUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaUitnodigenRapportage;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class MammaUitnodigenListener extends BaseLogListener
{
	private static final String RAPPORTAGE_ID_KEY = "rapportageId";

	public static final String INTERVAL_RAPPORTAGE_KEY = "intervalRapportageKey";

	private final HibernateService hibernateService;

	private final LogService logService;

	private final InstellingService instellingService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);
		jobExecution.getExecutionContext().put(MammaUitnodigenListener.INTERVAL_RAPPORTAGE_KEY, new HashMap<Long, MammaIntervalUitnodigenRapportage>());
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_UITNODIGEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_UITNODIGEN_AFGEROND;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new MammaUitnodigenLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = (MammaUitnodigenLogEvent) super.eindLogging(jobExecution);

		var executionContext = jobExecution.getExecutionContext();
		if (executionContext.containsKey(RAPPORTAGE_ID_KEY))
		{
			var rapportageId = executionContext.getLong(RAPPORTAGE_ID_KEY);
			var rapportage = hibernateService.get(MammaUitnodigenRapportage.class, rapportageId);

			intervalUitnodigingenBijwerkenInRapportage(rapportage);

			logEvent.setRapportage(rapportage);
		}

		return logEvent;
	}

	private void intervalUitnodigingenBijwerkenInRapportage(MammaUitnodigenRapportage eindRapportage)
	{
		Map<Long, MammaIntervalUitnodigenRapportage> intervalRapportages = getTypedValueFromExecutionContext(INTERVAL_RAPPORTAGE_KEY);
		for (var rapportageEntry : intervalRapportages.entrySet())
		{
			var screeningsorganisatie = hibernateService.get(ScreeningOrganisatie.class, rapportageEntry.getKey());
			var intervalRapportage = rapportageEntry.getValue();
			intervalRapportage.setScreeningOrganisatie(screeningsorganisatie);
			intervalRapportage.setUitnodigenRapportage(eindRapportage);
			eindRapportage.getIntervalUitnodigenRapportages().add(intervalRapportage);
			hibernateService.saveOrUpdateAll(intervalRapportage, eindRapportage);
		}
	}

	@Override
	protected void saveEindLogGebeurtenis(LogEvent logEvent)
	{
		final List<Instelling> dashboardOrganisaties = new ArrayList<>(instellingService.getActieveInstellingen(Rivm.class));

		var rapportage = ((MammaUitnodigenLogEvent) logEvent).getRapportage();
		if (rapportage != null)
		{
			var standplaatsUitnodigenRegios = rapportage.getStandplaatsRondeUitnodigenRapportages().stream().map(ur -> ur.getStandplaatsRonde().getStandplaats().getRegio());
			var intervalUitnodigenRegios = rapportage.getIntervalUitnodigenRapportages().stream().map(MammaIntervalUitnodigenRapportage::getScreeningOrganisatie);
			Stream.concat(standplaatsUitnodigenRegios, intervalUitnodigenRegios).distinct().forEach(dashboardOrganisaties::add);
		}

		logService.logGebeurtenis(getEindLogGebeurtenis(), dashboardOrganisaties, logEvent, getBevolkingsonderzoek());
	}
}
