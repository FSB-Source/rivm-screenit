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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.DashboardDao;
import nl.rivm.screenit.dao.LogDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.dashboard.DashboardActieType;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class DashboardServiceImpl implements DashboardService
{
	@Autowired
	private DashboardDao dashboardDao;

	@Autowired
	private LogDao logDao;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	@Override
	public List<DashboardStatus> getDashboardStatussen(DashboardType item)
	{
		return dashboardDao.getDashboardStatussen(item);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateDashboard(LogRegel logRegel, List<Instelling> dashboardOrganisaties)
	{
		LogEvent logEvent = logRegel.getLogEvent();
		if (logEvent != null)
		{
			DashboardType dashboardType = logRegel.getLogGebeurtenis().getDashboardType();
			if (dashboardType != null)
			{
				boolean resetDashboardLevel = false;
				switch (logRegel.getLogGebeurtenis().getDashboardActieType())
				{
				case DAG:
					Date dateTimeLastLogRegel = dashboardDao.getDateTimeLastLogRegel(dashboardType);
					if (dateTimeLastLogRegel != null && DateUtil.toLocalDate(dateTimeLastLogRegel).isBefore(dateSupplier.getLocalDate()))
					{
						dashboardDao.maakDashboardStatusLeeg(dashboardType);
						resetDashboardLevel = true;
					}
					break;
				case START:
					dashboardDao.maakDashboardStatusLeeg(dashboardType);
					resetDashboardLevel = true;
					break;
				case OVERIG:
					break;
				default:
					throw new IllegalStateException();
				}
				List<DashboardStatus> dashboardStatussen = dashboardDao.getDashboardStatussen(dashboardType);
				List<Instelling> instellingenNogGeenDashboardStatus = new ArrayList<>(dashboardOrganisaties);
				if (dashboardOrganisaties.isEmpty())
				{
					instellingenNogGeenDashboardStatus.add(hibernateService.loadAll(Rivm.class).get(0));
				}
				for (DashboardStatus dashboardStatus : dashboardStatussen)
				{
					if (resetDashboardLevel)
					{
						dashboardStatus.setLevel(Level.INFO);
					}
					Instelling dashboardVanOrganisatie = dashboardStatus.getOrganisatie();
					if (dashboardOrganisaties.contains(dashboardVanOrganisatie)
						|| dashboardOrganisaties.isEmpty() && dashboardVanOrganisatie.getOrganisatieType().equals(OrganisatieType.RIVM))
					{
						instellingenNogGeenDashboardStatus.remove(dashboardVanOrganisatie);
						maakDashboardLogRegel(logRegel, logEvent, dashboardStatus);
					}
					hibernateService.saveOrUpdate(dashboardStatus);
				}
				for (Instelling instellingNogGeenDashboardStatus : instellingenNogGeenDashboardStatus)
				{
					DashboardStatus nieuwDashboardStatus = maakDashboardStatus(dashboardType, instellingNogGeenDashboardStatus);
					maakDashboardLogRegel(logRegel, logEvent, nieuwDashboardStatus);
				}
			}
		}
	}

	private boolean downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(DashboardStatus dashboardStatus)
	{
		Level oldDashboardStatus = dashboardStatus.getLevel();
		Criteria crit = hibernateService.getHibernateSession().createCriteria(DashboardLogRegel.class);
		crit.add(Restrictions.eq("dashboardStatus.id", dashboardStatus.getId()));
		List<DashboardLogRegel> logRegelsVoorDashboard = crit.list();
		dashboardStatus.setLevel(Level.INFO);
		for (DashboardLogRegel dashboardLogRegel : logRegelsVoorDashboard)
		{
			switch (dashboardLogRegel.getLogRegel().getLogEvent().getLevel())
			{
			case ERROR:
				dashboardStatus.setLevel(Level.ERROR);
				hibernateService.saveOrUpdate(dashboardStatus);
				return false;
			case WARNING:
				dashboardStatus.setLevel(Level.WARNING);
				break;
			}
		}
		hibernateService.saveOrUpdate(dashboardStatus);
		return oldDashboardStatus != dashboardStatus.getLevel();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean updateLogRegelMetDashboardStatus(LogRegel logregel, String gebruikersnaam, DashboardStatus dashboardStatus)
	{
		logregel.getLogEvent().setLevel(Level.INFO);
		LOG.info("Gebruiker " + gebruikersnaam + " heeft aangemerkt logregel " + logregel.getId() + " te hebben gezien.");
		hibernateService.saveOrUpdate(logregel);

		boolean isGedowngrade = downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(dashboardStatus);
		downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(getDashboardStatussen(dashboardStatus.getType()));
		return isGedowngrade;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean verwijderLogRegelsVanDashboards(List<LogGebeurtenis> gebeurtenissen, String bsn, String melding)
	{
		LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();
		loggingZoekCriteria.setGebeurtenis(gebeurtenissen);
		loggingZoekCriteria.setBsnClient(bsn);
		loggingZoekCriteria.setMelding(melding);

		loggingZoekCriteria.setVanaf(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusDays(1)));

		List<LogRegel> logRegels = logDao.getLogRegels(loggingZoekCriteria, 0, Integer.MAX_VALUE, new SortState<>("gebeurtenisDatum", Boolean.FALSE));

		return verwijderLogRegelsVanDashboards(logRegels);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean verwijderLogRegelsVanDashboards(List<LogRegel> logRegels)
	{
		Set<DashboardLogRegel> dashboardLogRegels = logRegels.stream().flatMap(lr -> dashboardDao.getDashboardLogRegelMetLogRegel(lr).stream()).collect(Collectors.toSet());
		Set<DashboardStatus> dashboardStatussen = dashboardLogRegels.stream().map(DashboardLogRegel::getDashboardStatus).collect(Collectors.toSet());
		hibernateService.deleteAll(dashboardLogRegels);

		return downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(new ArrayList<>(dashboardStatussen));
	}

	private boolean downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(List<DashboardStatus> dashboardStatussen)
	{
		AtomicBoolean downgraded = new AtomicBoolean(false);
		dashboardStatussen.forEach(s -> downgraded.set(downgraded.get() || downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(s)));
		return downgraded.get();
	}

	private DashboardStatus maakDashboardStatus(DashboardType dashboardType, Instelling instellingNogGeenDashboardStatus)
	{
		DashboardStatus nieuwDashboardStatus = new DashboardStatus();
		nieuwDashboardStatus.setType(dashboardType);
		nieuwDashboardStatus.setOrganisatie(instellingNogGeenDashboardStatus);
		nieuwDashboardStatus.setLevel(Level.INFO);
		return nieuwDashboardStatus;
	}

	private void maakDashboardLogRegel(LogRegel logRegel, LogEvent logEvent, DashboardStatus dashboardStatus)
	{
		DashboardLogRegel dashboardLogRegel = new DashboardLogRegel();
		dashboardLogRegel.setDashboardStatus(dashboardStatus);
		dashboardLogRegel.setLogRegel(logRegel);
		if (dashboardStatus.getLevel() == null || logEvent.getLevel().ordinal() > dashboardStatus.getLevel().ordinal())
		{
			if (!Level.ERROR.equals(dashboardStatus.getLevel()) && logEvent.getLevel().equals(Level.ERROR))
			{
				sendErrorEmail(dashboardStatus);
			}
			dashboardStatus.setLevel(logEvent.getLevel());
		}
		else if (logRegel.getLogGebeurtenis().getDashboardActieType() == DashboardActieType.DAG
			&& logEvent.getLevel().equals(Level.ERROR))
		{
			sendErrorEmail(dashboardStatus);
		}
		hibernateService.saveOrUpdateAll(dashboardStatus, dashboardLogRegel);
	}

	private void sendErrorEmail(DashboardStatus dashboardStatus)
	{
		String emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
		if (emailadressen != null)
		{
			if (StringUtils.isNotBlank(dashboardStatus.getEmailadressen()))
			{
				emailadressen = dashboardStatus.getEmailadressen();
			}

			DashboardType type = dashboardStatus.getType();
			String content = "Dashboard " + type.getNaam() + " van " + dashboardStatus.getOrganisatie().getNaam() + " heeft een rode status gekregen.";

			mailService.queueMail(emailadressen,
				"ScreenIT " + type.getNaam() + "(" + Bevolkingsonderzoek.getAfkortingen(type.getBevolkingsOnderzoek()) + ")" + " gefaald op " + applicationEnvironment, content,
				MailPriority.HIGH);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Level getHoogsteLevelDashboardItems(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsOnderzoeken)
	{
		Level highestLevel = Level.INFO;
		List<DashboardStatus> statussen = getListOfDashboardStatussen(ingelogdVoorOrganisatie, bevolkingsOnderzoeken);
		for (DashboardStatus status : statussen)
		{
			if (highestLevel.ordinal() < status.getLevel().ordinal())
			{
				highestLevel = status.getLevel();
			}
		}
		return highestLevel;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<DashboardStatus> getListOfDashboardStatussen(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsOnderzoeken)
	{
		List<DashboardStatus> listOfDashboardStatussen = dashboardDao.getListOfDashboardStatussen(ingelogdVoorOrganisatie);
		List<DashboardStatus> listOfDashboardStatussenPerBVO = new ArrayList<>();
		for (DashboardType type : DashboardType.values())
		{
			boolean heeftBVO = false;
			for (Bevolkingsonderzoek bvo : type.getBevolkingsOnderzoek())
			{
				if (bevolkingsOnderzoeken.contains(bvo) || bevolkingsOnderzoeken.isEmpty())
				{
					heeftBVO = true;
					break;
				}
			}
			if (heeftBVO)
			{
				boolean foundType = false;
				for (DashboardStatus status : listOfDashboardStatussen)
				{
					if (status.getType().equals(type))
					{
						listOfDashboardStatussenPerBVO.add(status);
						foundType = true;
						break;
					}
				}
				if (!foundType)
				{
					if (ingelogdVoorOrganisatie != null && (ingelogdVoorOrganisatie.getOrganisatieType() == OrganisatieType.RIVM
						|| type.getOrganisatieTypes() != null && type.getOrganisatieTypes().contains(ingelogdVoorOrganisatie.getOrganisatieType())))
					{
						DashboardStatus dashboardStatus = maakDashboardStatus(type, ingelogdVoorOrganisatie);
						hibernateService.saveOrUpdate(dashboardStatus);
						listOfDashboardStatussenPerBVO.add(dashboardStatus);
					}
				}

			}
		}
		Collections.sort(listOfDashboardStatussenPerBVO, Comparator.comparingInt(o -> o.getType().ordinal()));

		return listOfDashboardStatussenPerBVO;
	}

}
