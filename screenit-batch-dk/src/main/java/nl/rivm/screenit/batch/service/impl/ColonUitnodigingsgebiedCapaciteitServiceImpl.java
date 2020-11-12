package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.datasource.ReadOnlyDBActionsWithFallback;
import nl.rivm.screenit.batch.datasource.ReadOnlyDBActionsWithFallback.DelegatedReadOnlyDBActions;
import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.batch.service.ColonUitnodigingsgebiedCapaciteitService;
import nl.rivm.screenit.dao.colon.impl.ColonClientSelectieHelper;
import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonUitnodigingsgebiedCapaciteitServiceImpl implements ColonUitnodigingsgebiedCapaciteitService
{
	public static final Logger LOG = LoggerFactory.getLogger(ColonUitnodigingsgebiedCapaciteitServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private PlanningService<VrijSlot> planningService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Autowired
	private ColonUitnodigingsgebiedService uitnodigingsGebiedService;

	@Autowired
	private LogService logService;

	@Override
	public Collection<ColonUitnodigingsgebiedSelectieContext> bepaalCapaciteit(ExecutionContext executionContext, boolean vooraankondigen)
	{
		List<ColonUitnodigingsgebiedSelectieContext> result = new ArrayList<>();
		ReadOnlyDBActionsWithFallback.runReadOnlyDBActions(new DelegatedReadOnlyDBActions()
		{

			@Override
			public void doActions()
			{
				LocalDateTime startTime = LocalDateTime.now();

				Map<Long, ColonUitnodigingsgebiedSelectieContext> uitnodigingsGebiedenMetCapaciteit = new HashMap<>();

				Map<Long, BigDecimal> gebiedsFactorCache = new HashMap<>();

				Integer percLandelijkIfobtRetour = preferenceService.getInteger(PreferenceKey.PERCENTAGEIFOBTRETOUR.name());
				if (percLandelijkIfobtRetour == null)
				{
					throw new IllegalStateException("Landelijk IfobtRetourPercentage is niet gezet");
				}

				Integer percLandelijkIfobtOngunstige = preferenceService.getInteger(PreferenceKey.PERCENTGAGEIFOBTONGUSTIG.name());
				if (percLandelijkIfobtOngunstige == null)
				{
					throw new IllegalStateException("Landelijk IfobtOngunstigePercentage is niet gezet");
				}

				DateTime geprognotiseerdeIntakeDatum = uitnodigingService.getGeprognotiseerdeIntakeDatum(vooraankondigen);

				DateTime weekStart = geprognotiseerdeIntakeDatum.dayOfWeek().withMinimumValue();
				DateTime weekEnd = geprognotiseerdeIntakeDatum.dayOfWeek().withMaximumValue();

				LOG.info("Bepaal uitnodigingscapaciteit op basis van de capaciteit van de intakelocaties voor alle uitnodigingsgebieden in de week van "
					+ weekStart.toString("dd-MM-yyyy hh:mm") + " tot/met " + weekEnd.toString("dd-MM-yyyy hh:mm"));
				List<ColoscopieCentrum> intakelocaties = instellingService.getActieveIntakelocaties();

				for (ColoscopieCentrum intakelocatie : intakelocaties)
				{
					List<VrijSlot> vrijeSloten = planningService.getBeschikbaarheid(weekStart.withTimeAtStartOfDay().toDate(), weekEnd.plusDays(1).withTimeAtStartOfDay().toDate(),
						intakelocatie);

					BigDecimal intakecapaciteitInWeek = BigDecimal.valueOf(vrijeSloten.size());

					DataSourceRouter.useReadWrite();

					for (VrijSlot vrijSlot : vrijeSloten)
					{
						RoosterItem roosterItem = hibernateService.load(RoosterItem.class, vrijSlot.getRoosterItemId());
						roosterItem.setCapaciteitMeeBepaald(true);
						hibernateService.saveOrUpdate(roosterItem);
					}
					DataSourceRouter.useReadOnly();

					BigDecimal intakecapaciteitPerDag = intakecapaciteitInWeek.divide(BigDecimal.valueOf(5), 2, RoundingMode.HALF_UP);

					LOG.info("Intakecapaciteit per dag in " + intakelocatie.getNaam() + ": " + BigDecimalUtil.decimalToString(intakecapaciteitPerDag));

					for (ColoscopieCentrumColonCapaciteitVerdeling capaciteitVerdeling : intakelocatie.getCapaciteitVerdeling())
					{
						UitnodigingsGebied uitnodigingsGebied = capaciteitVerdeling.getUitnodigingsGebied();
						if (uitnodigingsGebied.getGemeente() != null)
						{
							Long uitnodigingsGebiedId = uitnodigingsGebied.getId();
							BigDecimal gebiedsFactor = gebiedsFactorCache.get(uitnodigingsGebiedId);
							if (gebiedsFactor == null)
							{
								gebiedsFactor = uitnodigingsGebiedService.getIfobtFactorVoorGebied(uitnodigingsGebied, percLandelijkIfobtRetour, percLandelijkIfobtOngunstige);
								LOG.info("Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + ": Totaal factor " + BigDecimalUtil.decimalToString(gebiedsFactor));
								gebiedsFactorCache.put(uitnodigingsGebiedId, gebiedsFactor);
							}

							BigDecimal uitnodigingsCapaciteit = intakecapaciteitPerDag.multiply(gebiedsFactor);
							
							BigDecimal capVoorGebiedFactor = BigDecimal.valueOf(capaciteitVerdeling.getPercentageCapaciteit()).divide(BigDecimal.valueOf(10000), 4,
								RoundingMode.HALF_UP);

							BigDecimal uitnodigingsCapVanILVoorGebied = uitnodigingsCapaciteit.multiply(capVoorGebiedFactor);
							Long ilId = capaciteitVerdeling.getColoscopieCentrum().getId();
							LOG.info("Deel uitnodigingscapaciteit van intakelocatie " + intakelocatie.getNaam() + " naar uitnodigingsgebied " + uitnodigingsGebied.getNaam() + ": "
								+ BigDecimalUtil.decimalToString(capVoorGebiedFactor.multiply(BigDecimal.valueOf(100))) + "% van "
								+ BigDecimalUtil.decimalToString(uitnodigingsCapaciteit) + " => " + BigDecimalUtil.decimalToString(uitnodigingsCapVanILVoorGebied));

							if (!uitnodigingsGebiedenMetCapaciteit.containsKey(uitnodigingsGebiedId))
							{
								ColonUitnodigingsgebiedSelectieContext gebiedMetCapaciteit = new ColonUitnodigingsgebiedSelectieContext(uitnodigingsCapVanILVoorGebied, ilId,
									uitnodigingsGebiedId);
								uitnodigingsGebiedenMetCapaciteit.put(uitnodigingsGebiedId, gebiedMetCapaciteit);
							}
							else
							{
								ColonUitnodigingsgebiedSelectieContext gebiedMetCapaciteit = uitnodigingsGebiedenMetCapaciteit.get(uitnodigingsGebiedId);
								gebiedMetCapaciteit.addUitnodigingscapaciteit(uitnodigingsCapVanILVoorGebied, ilId, true);
							}
						}
					}
				}

				if (LOG.isDebugEnabled())
				{
					LOG.debug("berekening capaciteit duurde: " + ChronoUnit.MILLIS.between(startTime, LocalDateTime.now()) + " ms");
				}

				List<Long> uitnodigingsGebiedZonderRegio = new ArrayList<>();
				for (ColonUitnodigingsgebiedSelectieContext uitnodigingsgebiedSelectieContext : uitnodigingsGebiedenMetCapaciteit.values())
				{
					UitnodigingsGebied uitnodigingsGebied = hibernateService.load(UitnodigingsGebied.class, uitnodigingsgebiedSelectieContext.getUitnodigingsgebiedId());
					Gemeente gemeente = uitnodigingsGebied.getGemeente();
					if (gemeente.getScreeningOrganisatie() != null)
					{
						BigDecimal capaciteitOver = uitnodigingsgebiedSelectieContext.getUitnodigingscapaciteitOver();

						uitnodigingsgebiedSelectieContext.roundUitnodigingscapaciteitOver();

						if (LOG.isDebugEnabled())
						{
							LOG.debug("Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + " uitnodigingscapaciteit: "
								+ BigDecimalUtil.decimalToString(uitnodigingsgebiedSelectieContext.getUitnodigingscapaciteitOver()) + "("
								+ BigDecimalUtil.decimalToString(capaciteitOver) + ")");
						}
					}
					else
					{
						uitnodigingsGebiedZonderRegio.add(uitnodigingsgebiedSelectieContext.getUitnodigingsgebiedId());
						Map<Long, String> map = (Map<Long, String>) executionContext.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
						if (!executionContext.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
						{
							map = new HashMap<Long, String>();
							executionContext.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
						}
						map.put(gemeente.getId(), gemeente.getNaam());
						LOG.warn("Gemeente " + gemeente.getNaam() + " (" + gemeente.getId()
							+ ") is niet gekoppeld aan een screeningsorganisatie. Daardoor kunnen er geen uitnodigingen worden verstuurd naar clienten uit gebied "
							+ uitnodigingsGebied.getNaam() + ")");
					}
				}

				for (Long gebiedToRemove : uitnodigingsGebiedZonderRegio)
				{
					uitnodigingsGebiedenMetCapaciteit.remove(gebiedToRemove);
				}

				result.clear();
				result.addAll(uitnodigingsGebiedenMetCapaciteit.values());
				Collections.sort(result);

			}
		});
		DataSourceRouter.useReadWrite();
		return result;
	}

	@Override
	public List<ColonUitnodigingsgebiedSelectieContext> leegloopHerverdelen(Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden)
	{
		LOG.info("Zoek (nieuwe) leeglopende gebieden. En herverdelen indien nodig...");
		List<ColonUitnodigingsgebiedSelectieContext> leeglopendeGebieden = new ArrayList<>();
		for (ColonUitnodigingsgebiedSelectieContext gebiedContext : uitnodigingsgebieden)
		{
			UitnodigingsGebied uitnodigingsgebied = hibernateService.load(UitnodigingsGebied.class, gebiedContext.getUitnodigingsgebiedId());

			if (!gebiedContext.isLeeglopendGebied() && gebiedContext.isGenoegUitnodigingscapaciteitOver())
			{
				LOG.info("Leeglopend gebied " + uitnodigingsgebied.getNaam() + " " + gebiedContext);
				gebiedContext.setLeeglopendGebied(true);
				leeglopendeGebieden.add(gebiedContext);
			}
		}

		return herverdeelUitnodigingscapaciteitVanLegelopenedeGebieden(uitnodigingsgebieden, leeglopendeGebieden);
	}

	private List<ColonUitnodigingsgebiedSelectieContext> herverdeelUitnodigingscapaciteitVanLegelopenedeGebieden(
		Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden,
		List<ColonUitnodigingsgebiedSelectieContext> leeglopendeGebieden)
	{
		Set<ColonUitnodigingsgebiedSelectieContext> aangepasteGebieden = new HashSet<>();

		Map<Long, ColonUitnodigingsgebiedSelectieContext> alleGebiedenMap = new HashMap<>();
		for (ColonUitnodigingsgebiedSelectieContext gebied : uitnodigingsgebieden)
		{
			alleGebiedenMap.put(gebied.getUitnodigingsgebiedId(), gebied);
		}

		for (ColonUitnodigingsgebiedSelectieContext leeglopendGebied : leeglopendeGebieden)
		{
			UitnodigingsGebied leeglopendUitnodigingsgebied = hibernateService.load(UitnodigingsGebied.class, leeglopendGebied.getUitnodigingsgebiedId());

			for (ColoscopieCentrumColonCapaciteitVerdeling capVerdeling : leeglopendUitnodigingsgebied.getVerdeling())
			{
				ColoscopieCentrum intakelocatie = capVerdeling.getColoscopieCentrum();
				if (Boolean.TRUE.equals(intakelocatie.getActief()))
				{
					List<ColoscopieCentrumColonCapaciteitVerdeling> capaciteitVerdelingIL = intakelocatie.getCapaciteitVerdeling();
					List<ColoscopieCentrumColonCapaciteitVerdeling> targetGebieden = new ArrayList<>();

					for (ColoscopieCentrumColonCapaciteitVerdeling capVerdelingIL : capaciteitVerdelingIL)
					{
						ColonUitnodigingsgebiedSelectieContext gebied = alleGebiedenMap.get(capVerdelingIL.getUitnodigingsGebied().getId());
						if (gebied != null && !gebied.isLeeglopendGebied())
						{
							targetGebieden.add(capVerdelingIL);
						}
					}

					BigDecimal uitnodigingscapaciteitOverIL = leeglopendGebied.getUitnodigingscapaciteitOverVoorIntakelocatie(intakelocatie.getId());
					if (targetGebieden.isEmpty())
					{
						if (uitnodigingscapaciteitOverIL.compareTo(BigDecimal.ZERO) != 0)
						{
							logService.logGebeurtenis(LogGebeurtenis.SELECTIERONDE_CAPACITEIT_VAN_LEEG_GEBIED_GAAT_VERLOREN, null,
								"Gebied " + leeglopendUitnodigingsgebied.getNaam() + ", IL " + intakelocatie.getNaam() + ", uitnodigingscapaciteit over (#) "
									+ BigDecimalUtil.decimalToString(uitnodigingscapaciteitOverIL),
								Bevolkingsonderzoek.COLON);
						}
						LOG.info("Er gaat " + BigDecimalUtil.decimalToString(uitnodigingscapaciteitOverIL) + " aan uitnodigingscapaciteit verloren in gebied "
							+ leeglopendUitnodigingsgebied.getNaam() + " voor IL " + intakelocatie.getNaam());
					}
					else
					{

						Integer totaalPercentages = 0;
						for (ColoscopieCentrumColonCapaciteitVerdeling targetVerdeling : targetGebieden)
						{
							totaalPercentages += targetVerdeling.getPercentageCapaciteit();
						}
						if (totaalPercentages > 0)
						{

							for (ColoscopieCentrumColonCapaciteitVerdeling targetVerdeling : targetGebieden)
							{
								ColonUitnodigingsgebiedSelectieContext targetGebied = alleGebiedenMap.get(targetVerdeling.getUitnodigingsGebied().getId());

								BigDecimal capaciteitOmToeTeVoegen = uitnodigingscapaciteitOverIL.multiply(new BigDecimal(targetVerdeling.getPercentageCapaciteit()))
									.divide(new BigDecimal(totaalPercentages), 10, RoundingMode.HALF_UP);
								LOG.info("Er gaat " + BigDecimalUtil.decimalToString(capaciteitOmToeTeVoegen) + " aan uitnodigingscapaciteit van "
									+ leeglopendUitnodigingsgebied.getNaam() + " naar gebied " + targetVerdeling.getUitnodigingsGebied().getNaam() + " voor IL "
									+ targetVerdeling.getColoscopieCentrum().getNaam());
								targetGebied.addUitnodigingscapaciteit(capaciteitOmToeTeVoegen, targetVerdeling.getColoscopieCentrum().getId(), false);
								aangepasteGebieden.add(targetGebied);
							}
						}
						else
						{
							List<String> gebieden = new ArrayList<>();
							for (ColoscopieCentrumColonCapaciteitVerdeling targetVerdeling : targetGebieden)
							{
								gebieden.add(targetVerdeling.getUitnodigingsGebied().getNaam());
							}
							if (uitnodigingscapaciteitOverIL.compareTo(BigDecimal.ZERO) != 0)
							{
								logService.logGebeurtenis(LogGebeurtenis.SELECTIERONDE_CAPACITEIT_VAN_LEEG_GEBIED_GAAT_VERLOREN, null,
									"Gebied " + leeglopendUitnodigingsgebied.getNaam() + ", IL " + intakelocatie.getNaam() + ", uitnodigingscapaciteit over (#) "
										+ BigDecimalUtil.decimalToString(uitnodigingscapaciteitOverIL) + ", doelgebied(en) " + StringUtils.join(gebieden, ',')
										+ " geen percentage capaciteit toegekend",
									Bevolkingsonderzoek.COLON);
							}
							LOG.info("Er gaat " + BigDecimalUtil.decimalToString(uitnodigingscapaciteitOverIL) + " aan uitnodigingscapaciteit verloren in gebied "
								+ leeglopendUitnodigingsgebied.getNaam() + " voor IL " + intakelocatie.getNaam() + ", doelgebied(en) " + StringUtils.join(gebieden, ',')
								+ " geen percentage capaciteit toegekend");

						}
					}
				}
			}
		}

		List<ColonUitnodigingsgebiedSelectieContext> aangepasteUitnodigingsgebieden = new ArrayList<>(aangepasteGebieden);
		Collections.sort(aangepasteUitnodigingsgebieden);

		for (ColonUitnodigingsgebiedSelectieContext uitnodigingsgebiedSelectieContext : aangepasteUitnodigingsgebieden)
		{
			UitnodigingsGebied uitnodigingsgebied = hibernateService.load(UitnodigingsGebied.class, uitnodigingsgebiedSelectieContext.getUitnodigingsgebiedId());
			BigDecimal uitnodigingscapaciteitOver = uitnodigingsgebiedSelectieContext.getUitnodigingscapaciteitOver();

			uitnodigingsgebiedSelectieContext.roundUitnodigingscapaciteitOver();

			if (LOG.isDebugEnabled())
			{
				LOG.debug("Uitnodigingsgebied " + uitnodigingsgebied.getNaam() + " uitnodigingscapaciteit: "
					+ BigDecimalUtil.decimalToString(uitnodigingsgebiedSelectieContext.getUitnodigingscapaciteitOver())
					+ "(" + BigDecimalUtil.decimalToString(uitnodigingscapaciteitOver) + ")");
			}
		}
		return aangepasteUitnodigingsgebieden;
	}

	@Override
	public Integer bepaalProjectGroepPopulatie(Long uitnodigingsGebiedId, Long projectGroupId, Integer minimaleLeeftijd, Integer maximaleLeeftijd)
	{
		Integer maxAantalClienten = Integer.valueOf(0);
		UitnodigingsGebied uitnodigingsGebied = hibernateService.load(UitnodigingsGebied.class, uitnodigingsGebiedId);
		ProjectGroep projectGroep = hibernateService.load(ProjectGroep.class, projectGroupId);
		if (LOG.isDebugEnabled())
		{
			LOG.debug("Bepaal projectgroep populatie: Uitnodigingsgebied " + uitnodigingsGebied.getNaam() + "/project  " + projectGroep.getProject().getNaam() + "/groep "
				+ projectGroep.getNaam());
		}

		Criteria queryVooraankondigen = ColonClientSelectieHelper.getQueryVooraankondigen(hibernateService.getHibernateSession(), uitnodigingsGebied, null, true, minimaleLeeftijd,
			maximaleLeeftijd, projectGroupId, null);
		queryVooraankondigen.setProjection(Projections.projectionList().add(Projections.rowCount()));

		int aantalClientenInProjectGroep = ((Number) queryVooraankondigen.uniqueResult()).intValue();
		Date uitnodigenVoorDKvoor = projectGroep.getUitnodigenVoorDKvoor();

		int aantalWerkdagen = DateUtil.getDaysBetweenIgnoreWeekends(currentDateSupplier.getDateTimeMidnight(), new DateTime(uitnodigenVoorDKvoor), false);
		if (LOG.isDebugEnabled())
		{
			LOG.debug("Aantal clienten in uitnodigingsgebied " + uitnodigingsGebied.getNaam() + "/project " + projectGroep.getProject().getNaam()
				+ "/groep " + projectGroep.getNaam() + " is " + aantalClientenInProjectGroep + ". Aantal werkdagen: " + aantalWerkdagen);
		}
		if (aantalClientenInProjectGroep > 0)
		{
			if (aantalWerkdagen <= 0)
			{
				aantalWerkdagen = 1;
			}
			BigDecimal maxUitnodigingenPerDag = new BigDecimal(aantalClientenInProjectGroep).divide(new BigDecimal(aantalWerkdagen), RoundingMode.UP);

			if (maxUitnodigingenPerDag.compareTo(BigDecimal.ZERO) > 0)
			{
				maxAantalClienten = BigDecimalUtil.roundCapaciteit(maxUitnodigingenPerDag);
			}
			else
			{

			}
		}
		else
		{

		}
		if (LOG.isDebugEnabled())
		{
			LOG.debug("Max. aantal clienten in uitnodigingsgebied " + uitnodigingsGebied.getNaam() + "/project " + projectGroep.getProject().getNaam()
				+ "/groep " + projectGroep.getNaam() + " is vandaag " + maxAantalClienten);
		}

		return maxAantalClienten;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

	public void setPreferenceService(SimplePreferenceService preferenceService)
	{
		this.preferenceService = preferenceService;
	}

	public void setPlanningService(PlanningService<VrijSlot> planningService)
	{
		this.planningService = planningService;
	}

	public void setInstellingService(InstellingService instellingService)
	{
		this.instellingService = instellingService;
	}

	public void setColonUitnodigingService(ColonUitnodigingService colonUitnodigingService)
	{
		this.uitnodigingService = colonUitnodigingService;
	}

	public void setLogService(LogService logService)
	{
		this.logService = logService;
	}

	public void setUitnodigingsGebiedService(ColonUitnodigingsgebiedService uitnodigingsGebiedService)
	{
		this.uitnodigingsGebiedService = uitnodigingsGebiedService;
	}

}
