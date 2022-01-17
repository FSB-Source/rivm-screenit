package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import javax.annotation.PostConstruct;
import javax.persistence.Column;

import nl.rivm.screenit.dao.LogDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.loginformatie.model.Gebeurtenis;
import nl.topicuszorg.loginformatie.model.ILogInformatie;
import nl.topicuszorg.loginformatie.services.ILogInformatieService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;

@Service(value = "logInformatieService")
@Transactional(propagation = Propagation.SUPPORTS)
public class LogServiceImpl implements LogService, ILogInformatieService<ILogInformatie<?, ?, ?, ?, ?>, InstellingGebruiker, Gebeurtenis>
{

	private static final String MELDING_TOO_LONG_PREFIX = "...(voor de volledige informatie click op de regel)";

	private static final Logger LOG = LoggerFactory.getLogger(LogServiceImpl.class);

	private static int MELDING_COLUMN_SIZE;

	static
	{
		try
		{
			MELDING_COLUMN_SIZE = LogEvent.class.getDeclaredField("melding").getAnnotation(Column.class).length();
		}
		catch (NoSuchFieldException | SecurityException e)
		{
			LOG.error("Fout bij initialiseren van melding_kolom size", e);
		}
	}

	@Autowired
	private LogDao logDao;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired(required = false)
	@Qualifier("batchApplicationType")
	private String batchApplicationTypeString;

	@PostConstruct
	private void init()
	{
		Properties applicationProperties = new Properties();
		try (InputStream resourceAsStream = getClass().getResourceAsStream("/application.properties"))
		{
			applicationProperties.load(resourceAsStream);
			String version = applicationProperties.getProperty("application.version");
			String timestamp = applicationProperties.getProperty("application.timestamp");
			String buildnumber = applicationProperties.getProperty("application.buildnumber");
			LOG.info("ScreenIT versie: {} ({}, {})", version, buildnumber, timestamp);
		}
		catch (IOException e)
		{
			LOG.error("Fout bij het lezen van de application.properties", e);
		}
	}

	@Override
	public List<LogRegel> getLogRegelsVanDashboard(DashboardStatus item, int first, int count, SortState<String> sortState)
	{
		return logDao.getLogRegelsVanDashboard(item, first, count, sortState);
	}

	@Override
	public long countLogRegelsVanDashboard(DashboardStatus item)
	{
		return logDao.countLogRegelsVanDashboard(item);
	}

	@Override
	public List<LogRegel> getLogRegels(LoggingZoekCriteria loggingZoekCriteria, int first, int count, SortState<String> sortState)
	{
		return logDao.getLogRegels(loggingZoekCriteria, first, count, sortState);
	}

	@Override
	public long countLogRegels(LoggingZoekCriteria loggingZoekCriteria)
	{
		return logDao.countLogRegels(loggingZoekCriteria);
	}

	@Override
	public void inloggen(InstellingGebruiker ingelogd, Boolean uzipas)
	{

	}

	@Override
	public void saveOrUpdate(ILogInformatie<?, ?, ?, ?, ?> logInformatie)
	{
		if (logInformatie.getGebeurtenis() instanceof Gebeurtenis)
		{
			LogGebeurtenis logGebeurtenis = translate((Gebeurtenis) logInformatie.getGebeurtenis());
			if (logGebeurtenis != null)
			{
				LogEvent logEvent = getLogEvent(logGebeurtenis.getDefaultLevel(), null);
				logGebeurtenis(logGebeurtenis, logEvent, (Account) logInformatie.getOrganisatieMedewerkerRol(), getBvos(logGebeurtenis));
			}
		}
	}

	private Bevolkingsonderzoek[] getBvos(LogGebeurtenis logGebeurtenis)
	{
		Bevolkingsonderzoek[] bvos = null;
		if (LogGebeurtenis.MEDVRY_VERSTUURD.equals(logGebeurtenis) && batchApplicationTypeString != null)
		{
			BatchApplicationType batchApplicationType = BatchApplicationType.valueOf(batchApplicationTypeString);
			switch (batchApplicationType)
			{
			case CERVIX:
				bvos = new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX };
				break;
			case COLON:
				bvos = new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON };
				break;
			case GENERALIS:
				bvos = new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA };
				break;
			default:
				break;
			}
		}
		return bvos;
	}

	@Override
	public ILogInformatie<?, ?, ?, ?, ?> createLogInformatie(InstellingGebruiker ingelogd, Gebeurtenis gebeurtenis)
	{
		return null;
	}

	@Override
	public void createAndSaveLogInformatie(InstellingGebruiker ingelogd, Gebeurtenis gebeurtenis, String omschrijving)
	{
		LogGebeurtenis logGebeurtenis = translate(gebeurtenis);
		if (logGebeurtenis != null)
		{
			LogEvent logEvent = getLogEvent(logGebeurtenis.getDefaultLevel(), omschrijving);
			logGebeurtenis(logGebeurtenis, logEvent, ingelogd, getBvos(logGebeurtenis));
		}
	}

	private LogGebeurtenis translate(Gebeurtenis gebeurtenis)
	{
		switch (gebeurtenis)
		{
		case INLOGGEN:
			return LogGebeurtenis.INLOGGEN;
		case VERSTUREN_MEDVRY:
			return LogGebeurtenis.MEDVRY_VERSTUURD;
		default:
			return null;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), null);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, melding, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, client, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), null);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, client, melding, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, String melding,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, logEvent, null, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Client client,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, null, dashboardOrganisaties, logEvent, account, client, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis logGebeurtenis, LogEvent logEvent, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(logGebeurtenis, new ArrayList<>(), logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Client client, String melding)
	{
		logGebeurtenis(gebeurtenis, screeningsEenheid, dashboardOrganisaties, null, client, melding, null);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Account account, Client client,
		String melding, LocalDateTime datumTijd)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, screeningsEenheid, dashboardOrganisaties, logEvent, account, client, null, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, Account account, Client client, String melding, LocalDateTime datumTijd)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, screeningsEenheid, new ArrayList<>(), logEvent, account, client, datumTijd, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public void logGebeurtenis(LogGebeurtenis logGebeurtenis, MammaScreeningsEenheid mammaScreeningsEenheid, List<Instelling> dashboardOrganisaties, LogEvent logEvent,
		Account account, Client client, LocalDateTime datumTijd, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogRegel logRegel = new LogRegel();
		logRegel.setLogGebeurtenis(logGebeurtenis);
		if (datumTijd != null)
		{
			logRegel.setGebeurtenisDatum(DateUtil.toUtilDate(datumTijd));
		}
		else
		{
			logRegel.setGebeurtenisDatum(currentDateSupplier.getDate());
		}
		logRegel.setScreeningsEenheid(mammaScreeningsEenheid);
		logRegel.setGebruiker(getGebruiker(account));
		logRegel.setIngelogdeGebruiker(getIngelogdeGebruiker(account));
		if (logEvent != null && logEvent.getLevel().compareTo(logGebeurtenis.getDefaultLevel()) < 0)
		{
			logEvent.setLevel(logGebeurtenis.getDefaultLevel());
		}
		fitMelding(logEvent);

		logRegel.setLogEvent(logEvent);
		logEvent.setLogRegel(logRegel);
		if (client == null && account instanceof Client)
		{
			client = (Client) account;
		}
		logRegel.setClient(client);
		if (bevolkingsonderzoeken != null)
		{
			logRegel.setBevolkingsonderzoeken(Arrays.asList(bevolkingsonderzoeken));
		}
		else
		{
			logRegel.setBevolkingsonderzoeken(new ArrayList<>());
		}
		logDao.saveOrUpdateLogRegel(logRegel);

		dashboardService.updateDashboard(logRegel, dashboardOrganisaties);
	}

	@Override
	public boolean heeftBestaandeLogregelBinnenPeriode(List<LogGebeurtenis> gebeurtenis, String bsn, String melding, int dagen)
	{
		LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();
		loggingZoekCriteria.setGebeurtenis(gebeurtenis);
		loggingZoekCriteria.setBsnClient(bsn);
		loggingZoekCriteria.setMelding(melding);
		loggingZoekCriteria.setLevel(List.of(Level.INFO));

		loggingZoekCriteria.setVanaf(currentDateSupplier.getDateTime()
			.minusDays(dagen).toDate());

		List<LogRegel> result = getLogRegels(loggingZoekCriteria, 0, 1, new SortState<>("gebeurtenisDatum", Boolean.FALSE));

		return result.isEmpty();
	}

	private LogEvent getLogEvent(Level level, String melding)
	{
		LogEvent logEvent = new LogEvent();
		logEvent.setLevel(level);
		if (!Strings.isNullOrEmpty(melding))
		{
			logEvent.setMelding(melding);
		}

		return logEvent;
	}

	private void fitMelding(LogEvent logEvent)
	{
		String melding = logEvent.getMelding();
		if (melding != null && melding.length() > MELDING_COLUMN_SIZE)
		{
			logEvent.setVolledigeMelding(melding);
			boolean containsHTML = melding.matches(".*\\<[^>]+>.*");
			melding = melding.substring(0, MELDING_COLUMN_SIZE - MELDING_TOO_LONG_PREFIX.length());
			if (containsHTML && melding.lastIndexOf('<') > melding.lastIndexOf('>'))
			{
				melding = melding.substring(0, melding.lastIndexOf('<'));
			}
			melding += MELDING_TOO_LONG_PREFIX;
			logEvent.setMelding(melding);
		}
	}

	private Gebruiker getGebruiker(Account account)
	{
		if (account instanceof InstellingGebruiker)
		{
			return ((InstellingGebruiker) account).getMedewerker();
		}
		else if (account instanceof Gebruiker)
		{
			return (Gebruiker) account;
		}
		else
		{
			return null;
		}
	}

	private InstellingGebruiker getIngelogdeGebruiker(Account account)
	{
		if (account instanceof InstellingGebruiker)
		{
			return (InstellingGebruiker) account;
		}
		else
		{
			return null;
		}
	}

}
