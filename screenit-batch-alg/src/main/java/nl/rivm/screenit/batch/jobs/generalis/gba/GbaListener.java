package nl.rivm.screenit.batch.jobs.generalis.gba;

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
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.model.logging.GbaVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class GbaListener extends BaseLogListener
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		GbaVerwerkingsLog verwerkingsLog = new GbaVerwerkingsLog();
		verwerkingsLog.setDatumVerwerking(currentDateSupplier.getDate());
		jobExecution.getExecutionContext().put(GbaConstants.RAPPORTAGEKEYGBA, verwerkingsLog);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		LogEvent logEvent = new LogEvent();
		if (JobType.GBA_ZONDER_VO105.equals(getJobType()))
		{
			logEvent.setMelding("Zonder vo105");
		}
		return logEvent;
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.GBA_IMPORT_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.GBA_IMPORT_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		GbaVerwerkingBeeindigdLogEvent gbaVerwerkingBeeindigdLogEvent = new GbaVerwerkingBeeindigdLogEvent();
		ExecutionContext executionContext = getJobExecution().getExecutionContext();
		if (executionContext.containsKey(GbaConstants.RAPPORTAGEKEYGBA))
		{
			GbaVerwerkingsLog gbaVerwerkingsLog = (GbaVerwerkingsLog) executionContext.get(GbaConstants.RAPPORTAGEKEYGBA);
			gbaVerwerkingsLog.setAantalNieuweColonDossiers(executionContext.getLong(GbaConstants.AANTAL_COLON_DOSSIERS_KEY, 0));
			gbaVerwerkingsLog.setAantalNieuweCervixDossiers(executionContext.getLong(GbaConstants.AANTAL_CERVIX_DOSSIERS_KEY, 0));
			gbaVerwerkingsLog.setAantalNieuweMammaDossiers(executionContext.getLong(GbaConstants.AANTAL_MAMMA_DOSSIERS_KEY, 0));

			hibernateService.saveOrUpdate(gbaVerwerkingsLog);
			gbaVerwerkingBeeindigdLogEvent.setVerwerkingsLog(gbaVerwerkingsLog);
			return gbaVerwerkingBeeindigdLogEvent;
		}
		return null;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent logEvent = getEindLogEvent();
		if (logEvent != null)
		{
			GbaVerwerkingBeeindigdLogEvent gbaVerwerkingBeeindigdLogEvent = (GbaVerwerkingBeeindigdLogEvent) logEvent;
			GbaVerwerkingsLog gbaVerwerkingsLog = gbaVerwerkingBeeindigdLogEvent.getVerwerkingsLog();
			if (!gbaVerwerkingsLog.getFouten().isEmpty())
			{
				for (GbaFoutRegel fout : gbaVerwerkingsLog.getFouten())
				{
					if (GbaFoutCategorie.PROCES.equals(fout.getFoutCategorie()))
					{
						gbaVerwerkingBeeindigdLogEvent.setLevel(Level.ERROR);
						String melding = "";
						if (gbaVerwerkingBeeindigdLogEvent.getMelding() != null)
						{
							melding += gbaVerwerkingBeeindigdLogEvent.getMelding();
							melding += " - ";
						}
						melding += fout.getFout();
						gbaVerwerkingBeeindigdLogEvent.setMelding(melding);
						break;
					}
				}
				if (!Level.WARNING.equals(gbaVerwerkingBeeindigdLogEvent.getLevel()) && !Level.ERROR.equals(gbaVerwerkingBeeindigdLogEvent.getLevel()))
				{
					for (GbaFoutRegel fout : gbaVerwerkingsLog.getFouten())
					{
						if (GbaFoutCategorie.OVERIG.equals(fout.getFoutCategorie()))
						{
							gbaVerwerkingBeeindigdLogEvent.setLevel(Level.WARNING);
							String melding = "";
							if (gbaVerwerkingBeeindigdLogEvent.getMelding() != null)
							{
								melding += gbaVerwerkingBeeindigdLogEvent.getMelding();
								melding += " - ";
							}
							melding += fout.getFout();
							gbaVerwerkingBeeindigdLogEvent.setMelding(melding);
							break;
						}
					}
				}
			}
			if (jobHasExitCode(ExitStatus.FAILED) && gbaVerwerkingBeeindigdLogEvent.getVerwerkingsLog() != null
				&& CollectionUtils.isNotEmpty(jobExecution.getAllFailureExceptions()))
			{
				GbaFoutRegel foutRegel = new GbaFoutRegel();
				Throwable exception = jobExecution.getAllFailureExceptions().get(0);
				String error = exception.getMessage();
				gbaVerwerkingBeeindigdLogEvent.setLevel(Level.ERROR);
				if (StringUtils.isBlank(error) && ArrayUtils.isNotEmpty(exception.getStackTrace()))
				{
					error = getStackTrace(exception);
					gbaVerwerkingBeeindigdLogEvent.setMelding(error);
				}
				foutRegel.setFout(StringUtils.substring(error, 0, 1024));
				foutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
				foutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
				hibernateService.saveOrUpdate(foutRegel);

				gbaVerwerkingBeeindigdLogEvent.getVerwerkingsLog().getFouten().add(foutRegel);
				if (!gbaVerwerkingBeeindigdLogEvent.getVerwerkingsLog().getFouten().isEmpty())
				{
					gbaVerwerkingBeeindigdLogEvent.setMelding(gbaVerwerkingBeeindigdLogEvent.getVerwerkingsLog().getFouten().get(0).getFout());
				}
			}

			hibernateService.saveOrUpdate(gbaVerwerkingBeeindigdLogEvent);
			logEvent = gbaVerwerkingBeeindigdLogEvent;
		}
		else
		{
			logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("LogEvent niet gevonden in de executionContext terwijl deze wel wordt verwacht");
		}
		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}
}
