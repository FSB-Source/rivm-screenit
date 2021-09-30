package nl.rivm.screenit.mamma.se.proxy.cron;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.CleanUpService;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.TriggerContext;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.SchedulingConfigurer;
import org.springframework.scheduling.config.ScheduledTaskRegistrar;
import org.springframework.scheduling.support.CronTrigger;

@Configuration
@EnableScheduling
public class GegevensOphaalJob implements SchedulingConfigurer
{

	private static final Logger LOG = LoggerFactory.getLogger(GegevensOphaalJob.class);

	private static final String JOB_OMSCHRIJVING = "Ophalen gegevens";

	private static final int MAX_SE_AANTAL = 100;

	@Value("${MAX_MINUTEN_WACHTTIJD_GEGEVENS_OPHAAL_JOB:#{10}}")
	private int maxMinutenWachttijd;

	@Autowired
	private ConfiguratieService configuratieService;

	@Autowired
	private CleanUpService cleanUpService;

	@Autowired
	private SeStatusService seStatusService;

	@Override
	public void configureTasks(ScheduledTaskRegistrar scheduledTaskRegistrar)
	{
		LOG.info("GegevensOphaalJob draait met maxMinutenWachttijd: {}", maxMinutenWachttijd);
		scheduledTaskRegistrar.addTriggerTask(() -> {
			int delay = berekenWachttijd(seStatusService.getSeCode(), maxMinutenWachttijd);
			LOG.debug("Start: {} met maxMinutenWachttijd: {} op na {} ms {}", JOB_OMSCHRIJVING, maxMinutenWachttijd, delay, LocalDateTime.now().plus(delay, ChronoUnit.MILLIS));
			try
			{
				Thread.sleep(delay);
			}
			catch (InterruptedException e)
			{
				LOG.error("Fout opgetreden tijdens wachten ", e);
				return;
			}
			cleanUpService.nightlyCleanupAndRefresh();
			LOG.debug("Eind: " + JOB_OMSCHRIJVING);
		},
			this::haalGegevensOp);

	}

	private Date haalGegevensOp(TriggerContext triggerContext)
	{
		String cron = configuratieService.getConfiguratieValue(SeConfiguratieKey.SE_INFORMATIE_OPHALEN_CRON);

		if (cron == null)
		{
			cron = "0 0 2 * * ?";
		}

		CronTrigger trigger = new CronTrigger(cron);
		Date nextExec = trigger.nextExecutionTime(triggerContext);
		LOG.debug("Volgende job execution om " + nextExec + ". Gebruikte cron: " + cron);
		return nextExec;
	}

	int berekenWachttijd(String seCode, int maxMinutenWachttijd)
	{
		int seNr;
		try
		{
			seNr = Integer.parseInt(seCode.substring(3));
		}
		catch (Exception ex)
		{
			LOG.error("Ongeldig SE nummer '{}', gebruik delay van 0 ms", seCode);
			return 0;

		}
		int beschikbareMs = maxMinutenWachttijd * 60 * 1000;
		int msPerSlo = beschikbareMs / MAX_SE_AANTAL;
		return beschikbareMs > 0 ? (seNr * msPerSlo) % beschikbareMs : 0;
	}
}
