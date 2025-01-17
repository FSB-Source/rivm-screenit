package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import javax.annotation.PostConstruct;
import javax.persistence.Column;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.LogDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel_;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LogRegel_;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.repository.algemeen.DashboardLogRegelRepository;
import nl.rivm.screenit.repository.algemeen.LogRegelRepository;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.DashboardLogRegelSpecification.heeftDashboardType;
import static nl.rivm.screenit.specification.algemeen.DashboardLogRegelSpecification.heeftOrganisatie;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Service(value = "logInformatieService")
@Slf4j
public class LogServiceImpl implements LogService
{

	private static final String MELDING_TOO_LONG_PREFIX = "...(voor de volledige informatie click op de regel)";

	private static int meldingColumnSize;

	static
	{
		try
		{
			meldingColumnSize = LogEvent.class.getDeclaredField("melding").getAnnotation(Column.class).length();
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

	@Autowired
	private LogRegelRepository logRegelRepository;

	@Autowired
	private DashboardLogRegelRepository dashboardLogRegelRepository;

	@PostConstruct
	private void init()
	{
		var applicationProperties = new Properties();
		try (var resourceAsStream = getClass().getResourceAsStream("/build-info.properties"))
		{
			applicationProperties.load(resourceAsStream);
			String version = applicationProperties.getProperty("build.version");
			String timestamp = applicationProperties.getProperty("build.time");
			String buildnumber = applicationProperties.getProperty("build.number");
			LOG.info("ScreenIT versie: {} ({}, {})", version, buildnumber, timestamp);
		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden van build-info.properties (voor versienummer)");
		}
	}

	@Override
	public List<LogRegel> getLogRegelsVanDashboard(DashboardStatus item, long first, long count, Sort sort)
	{
		return dashboardLogRegelRepository.findWith(getLogRegelsSpecification(item), LogRegel.class,
			q -> q.projection((cb, r) -> r.get(DashboardLogRegel_.logRegel)).sortBy(sort, LogServiceImpl::sorteerLogRegels)).all(first, count);
	}

	private static Order sorteerLogRegels(Sort.Order order, Root<DashboardLogRegel> r, CriteriaBuilder cb)
	{
		if (order.getProperty().equals(propertyChain(LogRegel_.SCREENINGS_EENHEID, MammaScreeningsEenheid_.NAAM)))
		{
			var logregelJoin = join(r, DashboardLogRegel_.logRegel);
			join(logregelJoin, LogRegel_.screeningsEenheid, JoinType.LEFT);
		}
		return null;
	}

	@Override
	public List<LogRegel> getLogRegelsVanDashboard(DashboardStatus item)
	{

		return dashboardLogRegelRepository.findWith(getLogRegelsSpecification(item), LogRegel.class,
			q -> q.projection((cb, r) -> r.get(DashboardLogRegel_.logRegel))).all();
	}

	@Override
	public long countLogRegelsVanDashboard(DashboardStatus item)
	{
		return dashboardLogRegelRepository.count(getLogRegelsSpecification(item));
	}

	private Specification<DashboardLogRegel> getLogRegelsSpecification(DashboardStatus item)
	{
		return heeftDashboardType(item.getType()).and(heeftOrganisatie(item.getOrganisatie()));
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
	@Transactional
	public void createAndSaveLogInformatie(InstellingGebruiker ingelogd, LogGebeurtenis logGebeurtenis, String omschrijving)
	{
		if (logGebeurtenis != null)
		{
			LogEvent logEvent = getLogEvent(logGebeurtenis.getDefaultLevel(), omschrijving);
			logGebeurtenis(logGebeurtenis, logEvent, ingelogd, getBvos(logGebeurtenis));
		}
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), null);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, melding, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, client, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), null);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, new ArrayList<>(), account, client, melding, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, String melding,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, logEvent, null, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Client client,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, null, dashboardOrganisaties, logEvent, account, client, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(gebeurtenis, dashboardOrganisaties, logEvent, account, null, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis logGebeurtenis, LogEvent logEvent, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		logGebeurtenis(logGebeurtenis, new ArrayList<>(), logEvent, account, client, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Client client, String melding)
	{
		logGebeurtenis(gebeurtenis, screeningsEenheid, dashboardOrganisaties, null, client, melding, null);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Account account, Client client,
		String melding, LocalDateTime datumTijd)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, screeningsEenheid, dashboardOrganisaties, logEvent, account, client, datumTijd, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional
	public void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, Account account, Client client, String melding, LocalDateTime datumTijd,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		LogEvent logEvent = getLogEvent(gebeurtenis.getDefaultLevel(), melding);
		logGebeurtenis(gebeurtenis, screeningsEenheid, new ArrayList<>(), logEvent, account, client, datumTijd, bevolkingsonderzoeken);
	}

	@Override
	@Transactional
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
		if (logEvent.getLevel().compareTo(logGebeurtenis.getDefaultLevel()) < 0)
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

		logRegelRepository.save(logRegel);
		dashboardService.updateDashboard(logRegel, dashboardOrganisaties);
	}

	@Override
	public boolean heeftGeenBestaandeLogregelBinnenPeriode(List<LogGebeurtenis> gebeurtenissen, String bsn, String melding, int dagen)
	{
		return heeftGeenBestaandeLogregelBinnenPeriode(gebeurtenissen, bsn, List.of(Level.INFO), melding, dagen);
	}

	@Override
	public boolean heeftGeenBestaandeLogregelBinnenPeriode(List<LogGebeurtenis> gebeurtenissen, String bsn, List<Level> levels, String melding, int dagen)
	{
		LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();
		loggingZoekCriteria.setGebeurtenis(gebeurtenissen);
		loggingZoekCriteria.setBsnClient(bsn);
		loggingZoekCriteria.setMelding(melding);
		loggingZoekCriteria.setLevel(levels);

		loggingZoekCriteria.setVanaf(DateUtil.minDagen(currentDateSupplier.getDate(), dagen));

		List<LogRegel> result = getLogRegels(loggingZoekCriteria, 0, 1, new SortState<>("gebeurtenisDatum", Boolean.FALSE));

		return result.isEmpty();
	}

	@Override
	@Transactional
	public boolean verwijderLogRegelsVanDashboards(List<LogRegel> logRegels, InstellingGebruiker ingelogdeGebruiker, LogGebeurtenis logGebeurtenisVoorVerwijderActie)
	{
		if (!logRegels.isEmpty())
		{
			LogRegel logRegel = logRegels.get(0);
			logGebeurtenis(logGebeurtenisVoorVerwijderActie, ingelogdeGebruiker, logRegel.getClient(), logRegel.getBevolkingsonderzoeken().toArray(Bevolkingsonderzoek[]::new));
		}
		return dashboardService.verwijderLogRegelsVanDashboards(logRegels);
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
		if (melding != null && melding.length() > meldingColumnSize)
		{
			logEvent.setVolledigeMelding(melding);
			boolean containsHTML = melding.matches(".*\\<[^>]+>.*");
			melding = melding.substring(0, meldingColumnSize - MELDING_TOO_LONG_PREFIX.length());
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
