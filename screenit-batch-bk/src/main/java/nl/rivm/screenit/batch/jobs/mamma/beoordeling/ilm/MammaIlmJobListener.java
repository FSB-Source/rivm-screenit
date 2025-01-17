package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.batch.model.dto.MammaIlmRetryDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaIlmLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaIlmJobListener extends BaseLogListener
{
	public static final String KEY_BEELDEN_VERWIJDERD_AANTAL = "beeldenVerwijderdAantal";

	public static final String KEY_RONDES_VERWIJDERD_AANTAL = "rondesVerwijderdAantal";

	public static final String KEY_RONDES_VERWERKT_AANTAL = "rondesVerwerktAantal";

	public static final String KEY_BEELDEN_STATUS_ENTRIES = "beeldenStatusEntries";

	public static final String KEY_PALGA_VERSLAGEN_VERWIJDERD_AANTAL = "palgaVerslagenVerwijderdAantal";

	public static final String KEY_MAX_EIND_TIJD = "maxEindTijd";

	public static final String KEY_LAATSTE_RONDE_ID = "laatsteRondeId";

	public static final int MAX_AANTAL_RONDES_VERWERKEN_IN_STEP = 50; 

	private static final String REVISION_KENMERK_CONTEXT = "ILM";

	private final HibernateService hibernateService;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_BEELDEN_STATUS_SIGNALEREN_UITVOEREN);
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_GUNSTIGE_BEELDEN_VERWIJDEREN_UITVOEREN);
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_OVERIGE_BEELDEN_VERWIJDEREN_UITVOEREN);
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_PALGA_IMPORT_VERSLAGEN_VERWIJDEREN_UITVOEREN);
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_APPLICATIE_LOGGING_VERWIJDEREN_UITVOEREN);
		putOrganisatieParametersInExecutionContext(OrganisatieParameterKey.MAMMA_ILM_RONDES_VERWIJDEREN_UITVOEREN);
		putTimeInExecutionContext();
		jobExecution.getExecutionContext().putLong(KEY_LAATSTE_RONDE_ID, 0L);
		registerRevisionKenmerk(REVISION_KENMERK_CONTEXT, RevisionKenmerk.VERWIJDERD_DOOR_ILM);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_ILM_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_ILM_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new MammaIlmLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = (MammaIlmLogEvent) super.eindLogging(jobExecution);
		var executionContext = getJobExecution().getExecutionContext();
		addMelding(logEvent,
			"Rondes beelden verwijderd: " + (executionContext.containsKey(KEY_BEELDEN_VERWIJDERD_AANTAL) ? executionContext.getLong(KEY_BEELDEN_VERWIJDERD_AANTAL) : 0));

		addMelding(logEvent,
			"Rondes dossier verwijderd: " + (executionContext.containsKey(KEY_RONDES_VERWIJDERD_AANTAL) ? executionContext.getLong(KEY_RONDES_VERWIJDERD_AANTAL) : 0));

		addMelding(logEvent,
			"Palga verslagen verwijderd: "
				+ (executionContext.containsKey(KEY_PALGA_VERSLAGEN_VERWIJDERD_AANTAL) ? executionContext.getLong(KEY_PALGA_VERSLAGEN_VERWIJDERD_AANTAL) : 0));

		addRapportage(jobExecution, logEvent);
		hibernateService.saveOrUpdate(logEvent);

		unregisterRevisionKenmerk(REVISION_KENMERK_CONTEXT);

		return logEvent;
	}

	private void addRapportage(JobExecution jobExecution, MammaIlmLogEvent logEvent)
	{
		var statusRapportage = new MammaIlmBeeldenStatusRapportage();
		addEntries(jobExecution, statusRapportage);
		logEvent.setRapportage(statusRapportage);

		var aantalFailedRetries = getAantalFailedEntries(statusRapportage);
		var aantalRetries = getAantalRetries(statusRapportage);

		statusRapportage.setAantalRetries(aantalRetries);
		statusRapportage.setAantalFailedRetries(aantalFailedRetries);

		addMelding(logEvent, "Beelden verwijderen opnieuw geprobeerd : " + aantalRetries);
		addMelding(logEvent, "Beelden verwijderen opnieuw proberen gefaald: " + aantalFailedRetries);

		if (aantalFailedRetries > 0)
		{
			logEvent.setLevel(Level.WARNING);
		}

		hibernateService.saveOrUpdate(statusRapportage);
	}

	private long getAantalFailedEntries(MammaIlmBeeldenStatusRapportage rapportage)
	{
		return rapportage.getEntries().stream().filter(MammaIlmBeeldenStatusRapportageEntry::isFailedRetry).count();
	}

	private long getAantalRetries(MammaIlmBeeldenStatusRapportage rapportage)
	{
		return rapportage.getEntries().stream().filter(entry -> !entry.isFailedRetry()).count();
	}

	private void addEntries(JobExecution jobExecution, MammaIlmBeeldenStatusRapportage rapportage)
	{
		List<MammaIlmRetryDto> dtoList = (List<MammaIlmRetryDto>) jobExecution.getExecutionContext().get(KEY_BEELDEN_STATUS_ENTRIES);

		if (dtoList != null)
		{
			for (MammaIlmRetryDto dto : dtoList)
			{
				var client = hibernateService.get(Client.class, dto.getClientId());
				var rapportageEntry = new MammaIlmBeeldenStatusRapportageEntry(
					dto.getStatusDatum(),
					dto.getAccessionNumber(),
					client,
					dto.isBezwaar(),
					dto.isUploaded(),
					dto.isFailedRetry(),
					rapportage
				);
				rapportage.getEntries().add(rapportageEntry);
				hibernateService.saveOrUpdate(rapportageEntry);
			}
		}
	}

	private void putOrganisatieParametersInExecutionContext(OrganisatieParameterKey key)
	{
		getJobExecution().getExecutionContext().put(key.name(), organisatieParameterService.getOrganisatieParameter(null, key));
	}

	private void putTimeInExecutionContext()
	{
		Integer minutes = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_ILM_MAX_TIJD_MINUTEN);

		if (minutes != null)
		{
			Date startTime = new Date();
			Date endTime = DateUtil.plusTijdseenheid(startTime, minutes, ChronoUnit.MINUTES);
			getJobExecution().getExecutionContext().put(KEY_MAX_EIND_TIJD, endTime);
		}
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}
}
