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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.LogDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.dashboard.DashboardActieType;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel_;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LogRegel_;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.repository.algemeen.DashboardLogRegelRepository;
import nl.rivm.screenit.repository.algemeen.DashboardStatusRepository;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.DashboardLogRegelSpecification.heeftDashboardType;

@Slf4j
@Service
public class DashboardServiceImpl implements DashboardService
{
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

	@Autowired
	private DashboardLogRegelRepository dashboardLogRegelRepository;

	@Autowired
	private DashboardStatusRepository dashboardStatusRepository;

	@Override
	public List<DashboardStatus> getDashboardStatussen(DashboardType item)
	{
		return dashboardStatusRepository.findByType(item);
	}

	@Override
	@Transactional
	public void updateDashboard(LogRegel logRegel, List<Instelling> dashboardOrganisaties)
	{
		var logEvent = logRegel.getLogEvent();
		if (logEvent == null)
		{
			return;
		}

		var dashboardType = logRegel.getLogGebeurtenis().getDashboardType();
		if (dashboardType == null)
		{
			return;
		}

		var resetDashboardLevel = false;
		switch (logRegel.getLogGebeurtenis().getDashboardActieType())
		{
		case DAG:
			var dateTimeLastLogRegel = getLaatsteLogRegel(dashboardType);
			if (dateTimeLastLogRegel.isPresent() && DateUtil.toLocalDate(dateTimeLastLogRegel.get()).isBefore(dateSupplier.getLocalDate()))
			{
				resetDashboardLevel = verwijderLogRegelsEnResetDashboardStatus(dashboardType);
			}
			break;
		case START:
			resetDashboardLevel = verwijderLogRegelsEnResetDashboardStatus(dashboardType);
			break;
		case OVERIG:
			break;
		default:
			throw new IllegalStateException();
		}

		var dashboardStatussen = getDashboardStatussen(dashboardType);
		var instellingenNogGeenDashboardStatus = new ArrayList<>(dashboardOrganisaties);
		if (dashboardOrganisaties.isEmpty())
		{
			instellingenNogGeenDashboardStatus.add(hibernateService.loadAll(Rivm.class).get(0));
		}

		for (var dashboardStatus : dashboardStatussen)
		{
			if (resetDashboardLevel)
			{
				dashboardStatus.setLevel(Level.INFO);
			}
			var dashboardVanOrganisatie = dashboardStatus.getOrganisatie();
			if (dashboardOrganisaties.contains(dashboardVanOrganisatie)
				|| dashboardOrganisaties.isEmpty() && dashboardVanOrganisatie.getOrganisatieType().equals(OrganisatieType.RIVM))
			{
				instellingenNogGeenDashboardStatus.remove(dashboardVanOrganisatie);
				maakDashboardLogRegel(logRegel, logEvent, dashboardStatus);
			}
		}

		for (var instellingNogGeenDashboardStatus : instellingenNogGeenDashboardStatus)
		{
			var nieuwDashboardStatus = maakDashboardStatus(dashboardType, instellingNogGeenDashboardStatus);
			maakDashboardLogRegel(logRegel, logEvent, nieuwDashboardStatus);
		}
	}

	private Optional<Date> getLaatsteLogRegel(DashboardType dashboardType)
	{
		return dashboardLogRegelRepository.findWith(heeftDashboardType(dashboardType), Date.class, q -> q.projection((cb, r) ->
			cb.greatest(join(r, DashboardLogRegel_.logRegel).get(LogRegel_.gebeurtenisDatum))
		)).one();
	}

	private boolean verwijderLogRegelsEnResetDashboardStatus(DashboardType dashboardType)
	{
		maakDashboardStatusLeeg(dashboardType);
		return !dashboardLogRegelRepository.existsByDashboardStatusType(dashboardType);
	}

	private void maakDashboardStatusLeeg(DashboardType dashboardType)
	{
		if (dashboardType == DashboardType.GBA_LANDELIJK)
		{
			dashboardLogRegelRepository.maakDashboardStatusLeegVoorLandelijk(dashboardType, Level.INFO);
		}
		else
		{
			dashboardLogRegelRepository.maakDashboardStatusLeeg(dashboardType);
		}
	}

	private boolean downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(DashboardStatus dashboardStatus)
	{
		var oldDashboardStatus = dashboardStatus.getLevel();
		var logRegelsVoorDashboard = dashboardLogRegelRepository.findByDashboardStatus(dashboardStatus);
		dashboardStatus.setLevel(Level.INFO);
		for (var dashboardLogRegel : logRegelsVoorDashboard)
		{
			switch (dashboardLogRegel.getLogRegel().getLogEvent().getLevel())
			{
			case ERROR:
				dashboardStatus.setLevel(Level.ERROR);
				dashboardStatusRepository.save(dashboardStatus);
				return false;
			case WARNING:
				dashboardStatus.setLevel(Level.WARNING);
				dashboardStatusRepository.save(dashboardStatus);
				break;
			}
		}
		return oldDashboardStatus != dashboardStatus.getLevel();
	}

	@Override
	@Transactional
	public boolean updateLogRegelMetDashboardStatus(LogRegel logregel, String gebruikersnaam, DashboardStatus dashboardStatus)
	{
		logregel.getLogEvent().setLevel(Level.INFO);
		LOG.info("Gebruiker heeft aangemerkt logregel {} te hebben gezien.", logregel.getId());
		hibernateService.saveOrUpdate(logregel);
		var isGedowngrade = downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(dashboardStatus);
		downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(getDashboardStatussen(dashboardStatus.getType()));
		return isGedowngrade;
	}

	@Override
	@Transactional
	public boolean verwijderLogRegelsVanDashboards(List<LogGebeurtenis> gebeurtenissen, String bsn, String melding)
	{
		var loggingZoekCriteria = new LoggingZoekCriteria();
		loggingZoekCriteria.setGebeurtenis(gebeurtenissen);
		loggingZoekCriteria.setBsnClient(bsn);
		loggingZoekCriteria.setMelding(melding);

		loggingZoekCriteria.setVanaf(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusDays(1)));

		var logRegels = logDao.getLogRegels(loggingZoekCriteria, 0, Integer.MAX_VALUE, new SortState<>("gebeurtenisDatum", Boolean.FALSE));

		return verwijderLogRegelsVanDashboards(logRegels);
	}

	@Override
	@Transactional
	public boolean verwijderLogRegelsVanDashboards(List<LogRegel> logRegels)
	{
		var dashboardLogRegels = logRegels.stream().flatMap(lr -> getDashboardLogRegelMetLogRegel(lr).stream()).collect(Collectors.toSet());
		var dashboardStatussen = dashboardLogRegels.stream().map(DashboardLogRegel::getDashboardStatus).collect(Collectors.toSet());
		dashboardLogRegelRepository.deleteAll(dashboardLogRegels);

		return downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(new ArrayList<>(dashboardStatussen));
	}

	@Override
	public List<DashboardLogRegel> getDashboardLogRegelMetLogRegel(LogRegel logRegel)
	{
		return dashboardLogRegelRepository.findByLogRegel(logRegel);
	}

	private boolean downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(List<DashboardStatus> dashboardStatussen)
	{
		var downgraded = new AtomicBoolean(false);
		dashboardStatussen.forEach(s -> downgraded.set(downgraded.get() || downgradeDashboardStatussenLevelNaarBenedenAlsMogelijk(s)));
		return downgraded.get();
	}

	private DashboardStatus maakDashboardStatus(DashboardType dashboardType, Instelling instellingNogGeenDashboardStatus)
	{
		var nieuwDashboardStatus = new DashboardStatus();
		nieuwDashboardStatus.setType(dashboardType);
		nieuwDashboardStatus.setOrganisatie(instellingNogGeenDashboardStatus);
		nieuwDashboardStatus.setLevel(Level.INFO);
		return nieuwDashboardStatus;
	}

	private void maakDashboardLogRegel(LogRegel logRegel, LogEvent logEvent, DashboardStatus dashboardStatus)
	{
		var dashboardLogRegel = new DashboardLogRegel();
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
		var emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
		if (emailadressen != null)
		{
			if (StringUtils.isNotBlank(dashboardStatus.getEmailadressen()))
			{
				emailadressen = dashboardStatus.getEmailadressen();
			}

			var type = dashboardStatus.getType();
			var content = "Dashboard " + type.getNaam() + " van " + dashboardStatus.getOrganisatie().getNaam() + " heeft een rode status gekregen.";

			mailService.queueMailAanProfessional(emailadressen,
				"ScreenIT " + type.getNaam() + "(" + Bevolkingsonderzoek.getAfkortingen(type.getBevolkingsOnderzoek()) + ")" + " gefaald op " + applicationEnvironment, content,
				MailPriority.HIGH);
		}
	}

	@Override
	@Transactional
	public Level getHoogsteLevelDashboardItems(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsOnderzoeken)
	{
		var highestLevel = Level.INFO;
		var levels = List.of(Level.WARNING, Level.ERROR);
		var statussen = getListOfDashboardStatussen(ingelogdVoorOrganisatie, bevolkingsOnderzoeken, levels);
		for (var status : statussen)
		{
			if (highestLevel.ordinal() < status.getLevel().ordinal())
			{
				highestLevel = status.getLevel();
			}
		}
		return highestLevel;
	}

	@Override
	@Transactional
	public List<DashboardStatus> getListOfDashboardStatussen(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsonderzoeken, List<Level> loggingLevels)
	{
		var listOfDashboardStatussen = getDashboardStatussenVoorOrganisatie(ingelogdVoorOrganisatie);
		List<DashboardStatus> listOfDashboardStatussenPerBVO = new ArrayList<>();
		for (var type : DashboardType.values())
		{
			var heeftBVO = false;
			for (var bvo : type.getBevolkingsOnderzoek())
			{
				if (bevolkingsonderzoeken.contains(bvo) || bevolkingsonderzoeken.isEmpty())
				{
					heeftBVO = true;
					break;
				}
			}
			if (heeftBVO)
			{
				var foundType = false;
				for (var status : listOfDashboardStatussen)
				{
					if (status.getType().equals(type))
					{
						if (loggingLevels != null && loggingLevels.contains(status.getLevel()))
						{
							listOfDashboardStatussenPerBVO.add(status);
						}
						foundType = true;
						break;
					}
				}
				if (!foundType)
				{
					if (ingelogdVoorOrganisatie != null && (ingelogdVoorOrganisatie.getOrganisatieType() == OrganisatieType.RIVM
						|| type.getOrganisatieTypes() != null && type.getOrganisatieTypes().contains(ingelogdVoorOrganisatie.getOrganisatieType())))
					{
						var dashboardStatus = maakDashboardStatus(type, ingelogdVoorOrganisatie);
						hibernateService.saveOrUpdate(dashboardStatus);
						if (loggingLevels != null && loggingLevels.contains(dashboardStatus.getLevel()))
						{
							listOfDashboardStatussenPerBVO.add(dashboardStatus);
						}
					}
				}

			}
		}

		listOfDashboardStatussenPerBVO.sort(Comparator.comparingInt(o -> o.getType().ordinal()));
		return listOfDashboardStatussenPerBVO;
	}

	private List<DashboardStatus> getDashboardStatussenVoorOrganisatie(Instelling organisatie)
	{
		return dashboardStatusRepository.findByOrganisatie(organisatie);
	}
}
