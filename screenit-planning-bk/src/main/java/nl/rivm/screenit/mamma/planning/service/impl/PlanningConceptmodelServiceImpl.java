package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.planning.dao.PlanningAfspraakDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningBlokkadeDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningCapaciteitBlokDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningClientDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningScreeningRondeDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningScreeningsEenheidDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsPeriodeDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsRondeDao;
import nl.rivm.screenit.mamma.planning.dao.PlanningTehuisDao;
import nl.rivm.screenit.mamma.planning.dao.dto.BlokkadeDaoDto;
import nl.rivm.screenit.mamma.planning.dao.dto.ClientDaoDto;
import nl.rivm.screenit.mamma.planning.dao.dto.StandplaatsPeriodeDaoDto;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokkadeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientFactorTypeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientZonderPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsPeriodeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsRondeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStatusIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningTehuisIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;
import nl.rivm.screenit.mamma.planning.repository.PlanningPostcodeReeksRepository;
import nl.rivm.screenit.mamma.planning.repository.PlanningStandplaatsPeriodeRepository;
import nl.rivm.screenit.mamma.planning.service.PlanningCapaciteitAgendaService;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptmodelService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.repository.algemeen.ScreeningOrganisatieRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@RequiredArgsConstructor
public class PlanningConceptmodelServiceImpl implements PlanningConceptmodelService, ApplicationListener<ContextRefreshedEvent>
{
	private final Map<Long, Set<Long>> teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId = new HashMap<>();

	private final Map<Long, Set<Long>> teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId = new HashMap<>();

	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService simplePreferenceService;

	private final MammaBaseAfspraakService afspraakService;

	private final PlanningCapaciteitAgendaService capaciteitAgendaService;

	private final PlanningConceptOpslaanService conceptOpslaanService;

	private final PlanningCapaciteitBlokDao capaciteitBlokDao;

	private final Environment env;

	private final ScreeningOrganisatieRepository screeningOrganisatieRepository;

	private final PlanningScreeningsEenheidDao planningScreeningsEenheidDao;

	private final PlanningStandplaatsPeriodeDao planningStandplaatsPeriodeDao;

	private final PlanningCapaciteitBlokDao planningCapaciteitBlokDao;

	private final PlanningStandplaatsDao planningStandplaatsDao;

	private final PlanningPostcodeReeksRepository planningPostcodeReeksRepository;

	private final PlanningStandplaatsRondeDao planningStandplaatsRondeDao;

	private final PlanningStandplaatsPeriodeRepository planningStandplaatsPeriodeRepository;

	private final PlanningTehuisDao planningTehuisDao;

	private final PlanningClientDao planningClientDao;

	private final PlanningScreeningRondeDao planningScreeningRondeDao;

	private final PlanningBlokkadeDao planningBlokkadeDao;

	private final PlanningAfspraakDao planningAfspraakDao;

	private boolean doInit = true; 

	private LocalDate herhalenVanaf;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onApplicationEvent(@NonNull ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit && Arrays.stream(env.getActiveProfiles()).noneMatch(s -> s.equals("test")))
		{
			LOG.info("doInit");
			try
			{
				PlanningStatusIndex.set(MammaPlanningStatus.OPSTARTEN);
				resetConceptmodel();
				PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			}
			catch (Exception e)
			{
				LOG.error("Fout bij opstarten", e);
				PlanningStatusIndex.set(MammaPlanningStatus.ERROR);
			}
			finally
			{
				doInit = false;
			}
		}
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void resetConceptmodel()
	{
		LOG.info("resetConceptmodel");

		maakConceptmodelLeeg();

		leesScreeningsOrganisaties();
		leesConstanten();

		for (var planningScreeningsOrganisatie : PlanningScreeningsOrganisatieIndex.getScreeningsOrganisaties())
		{
			leesScreeningsEenheden(planningScreeningsOrganisatie);
			leesStandplaatsen(planningScreeningsOrganisatie);
		}

		leesStandplaatsRonden(teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.values().stream().flatMap(Set::stream).collect(Collectors.toSet()));
		leesStandplaatsPerioden(teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.values().stream().flatMap(Set::stream).collect(Collectors.toSet()));

		leesTehuizen();
		leesClienten();
		leesBlokkades();
		leesGebruikteCapaciteit();

		capaciteitAgendaService.herhalen(herhalenVanaf);
		conceptOpslaanService.slaConceptOpVoorAlleScreeningsOrganisaties();
	}

	private static void maakConceptmodelLeeg()
	{
		PlanningWijzigingen.clear();

		PlanningStandplaatsPeriodeIndex.clear();
		PlanningStandplaatsRondeIndex.clear();
		PlanningScreeningsOrganisatieIndex.clear();
		PlanningScreeningsEenheidIndex.clear();
		PlanningBlokIndex.removeAll();
		PlanningBlokkadeIndex.clear();
		PlanningClientZonderPostcodeReeksIndex.clear();
		PlanningPostcodeReeksIndex.clear();
		PlanningStandplaatsIndex.clear();
		PlanningClientIndex.clear();
		PlanningClientFactorTypeIndex.clear();
		PlanningTehuisIndex.clear();
	}

	@Override
	public void annuleerConceptmodelWijzigingen(PlanningScreeningsOrganisatie planningScreeningsOrganisatie)
	{
		LOG.info("annuleerConceptmodelWijzigingen voor so: {}", planningScreeningsOrganisatie.getId());

		for (var planningScreeningsEenheid : planningScreeningsOrganisatie.getScreeningsEenheidSet())
		{
			for (var planningStandplaatsPeriode : planningScreeningsEenheid.getStandplaatsPeriodeNavigableSet())
			{
				PlanningStandplaatsPeriodeIndex.remove(planningStandplaatsPeriode);
			}

			for (var planningDag : planningScreeningsEenheid.getDagNavigableMap().values())
			{
				planningDag.getBlokSet().clear();
				planningDag.setStandplaatsPeriode(null);
			}

			PlanningBlokIndex.removeAll(planningScreeningsEenheid);
			planningScreeningsEenheid.getBlokSet().clear();
			planningScreeningsEenheid.getStandplaatsPeriodeNavigableSet().clear();
			planningScreeningsEenheid.setHerhalingsWeek(planningScreeningsEenheid.getInitieelHerhalingsWeek());
		}

		for (var planningStandplaats : planningScreeningsOrganisatie.getStandplaatsSet())
		{
			planningStandplaats.getStandplaatsRondeNavigableSet().clear();
		}

		planningScreeningsOrganisatie.getScreeningsEenheidSet().forEach(this::leesBeschikbareCapaciteit);
		planningScreeningsOrganisatie.resetConceptGewijzigdDoor();
		leesStandplaatsRonden(teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.get(planningScreeningsOrganisatie.getId()));
		leesStandplaatsPerioden(teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.get(planningScreeningsOrganisatie.getId()));

		PlanningDoorrekenenManager.run();
	}

	@Override
	public void addStandplaatsPeriode(PlanningStandplaatsPeriode planningStandplaatsPeriode)
	{
		Long screeningsOrganisatieId = planningStandplaatsPeriode.getScreeningsEenheid().getScreeningsOrganisatie().getId();
		teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.get(screeningsOrganisatieId).add(planningStandplaatsPeriode.getId());
		teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.get(screeningsOrganisatieId).add(planningStandplaatsPeriode.getStandplaatsRonde().getId());
	}

	private void leesScreeningsOrganisaties()
	{
		LOG.info("leesScreeningsOrganisaties");

		teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.clear();
		teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.clear();

		var persistentScreeningOrganisaties = screeningOrganisatieRepository.findAllByActiefTrueOrderByNaam();
		for (var persistentScreeningOrganisatie : persistentScreeningOrganisaties)
		{
			PlanningScreeningsOrganisatieIndex.put(new PlanningScreeningsOrganisatie(persistentScreeningOrganisatie));
			teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.put(persistentScreeningOrganisatie.getId(), new HashSet<>());
			teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.put(persistentScreeningOrganisatie.getId(), new HashSet<>());
		}

		LOG.info("{} so's gelezen", persistentScreeningOrganisaties.size());
	}

	private void leesConstanten()
	{
		LOG.info("leesConstanten");

		var actieveStandplaatsPeriodeDtos = leesActieveStandplaatsPeriodes();

		vulTeLezenStandplaatsPeriodesEnRondes(actieveStandplaatsPeriodeDtos);

		var plannenVanaf = beginDatumEersteStandplaatsPeriodeMinusWekenVanTeVorenUitnodigen(actieveStandplaatsPeriodeDtos);
		var eindDatumLaatsteStandplaatsPeriode = eindDatumLaatsteStandplaatsPeriode(actieveStandplaatsPeriodeDtos);
		var datumLaatsteCapaciteitsBlok = planningCapaciteitBlokDao.findMaxDatumVanAlleCapaciteitsBlokken().orElse(dateSupplier.getLocalDate());

		herhalenVanaf = datumLaatsteCapaciteitsBlok.plusWeeks(1).with(DayOfWeek.MONDAY);

		var plannenTotEnMet = Collections.max(List.of(
			eindDatumLaatsteStandplaatsPeriode.plusMonths(6),
			plannenVanaf.plusYears(2), 
			datumLaatsteCapaciteitsBlok 
		));

		LOG.info("Constanten: plannenVanaf: {}, eindDatumLaatsteStandplaatsPeriode: {}, datumLaatsteCapaciteitsBlok: {},  plannenTotEnMet: {}, herhalenVanaf: {}",
			plannenVanaf, eindDatumLaatsteStandplaatsPeriode, datumLaatsteCapaciteitsBlok, plannenTotEnMet, herhalenVanaf);

		var vanafLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
		var totEnMetLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());

		PlanningConstanten.set(plannenVanaf, plannenTotEnMet, dateSupplier.getLocalDateTime(), vanafLeeftijd, totEnMetLeeftijd);
	}

	private LocalDate beginDatumEersteStandplaatsPeriodeMinusWekenVanTeVorenUitnodigen(List<StandplaatsPeriodeDaoDto> actieveStandplaatsPeriodeDtos)
	{
		return actieveStandplaatsPeriodeDtos.stream()
			.map(spp -> spp.getVanaf().minusWeeks(spp.getWekenVanTevorenUitnodigen()))
			.min(Comparator.naturalOrder())
			.orElse(dateSupplier.getLocalDate());
	}

	private LocalDate eindDatumLaatsteStandplaatsPeriode(List<StandplaatsPeriodeDaoDto> actieveStandplaatsPeriodeDtos)
	{
		return actieveStandplaatsPeriodeDtos.stream()
			.map(StandplaatsPeriodeDaoDto::getTotEnMet)
			.max(Comparator.naturalOrder())
			.orElse(dateSupplier.getLocalDate());
	}

	private List<StandplaatsPeriodeDaoDto> leesActieveStandplaatsPeriodes()
	{
		var volgnummersEersteActieveStandplaatsPeriodePerSe = planningScreeningsEenheidDao.volgnummersEersteActieveStandplaatsPeriodePerSe();
		return planningStandplaatsPeriodeDao.standplaatsPeriodesVanafVolgnummer(volgnummersEersteActieveStandplaatsPeriodePerSe);
	}

	private void vulTeLezenStandplaatsPeriodesEnRondes(List<StandplaatsPeriodeDaoDto> actieveStandplaatsPeriodeDtos)
	{
		teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.forEach((k, v) -> v.clear());
		teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.forEach((k, v) -> v.clear());

		for (var standplaatsPeriodeDto : actieveStandplaatsPeriodeDtos)
		{
			teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.get(standplaatsPeriodeDto.getRegioId()).add(standplaatsPeriodeDto.getStandplaatsPeriodeId());
			teLezenStandplaatsRondeIdsPerScreeningsOrganisatieId.get(standplaatsPeriodeDto.getRegioId()).add(standplaatsPeriodeDto.getStandplaatsRondeId());
		}
	}

	private void leesScreeningsEenheden(PlanningScreeningsOrganisatie planningScreeningsOrganisatie)
	{
		LOG.info("leesScreeningsEenheden so: {}", planningScreeningsOrganisatie.getId());

		var planningScreeningsEenheden = planningScreeningsEenheidDao.actieveScreeningsEenhedenVanScreeningsOrganisatie(planningScreeningsOrganisatie);

		for (var planningScreeningsEenheid : planningScreeningsEenheden)
		{
			planningScreeningsEenheid.setScreeningsOrganisatie(planningScreeningsOrganisatie);
			planningScreeningsOrganisatie.getScreeningsEenheidSet().add(planningScreeningsEenheid);
			leesBeschikbareCapaciteit(planningScreeningsEenheid);
			PlanningScreeningsEenheidIndex.put(planningScreeningsEenheid);
		}

		LOG.info("{} se's gelezen voor so: {}", planningScreeningsEenheden.size(), planningScreeningsOrganisatie.getId());
	}

	private void leesBeschikbareCapaciteit(PlanningScreeningsEenheid planningScreeningsEenheid)
	{
		LOG.info("leesBeschikbareCapaciteit se: {}", planningScreeningsEenheid.getId());

		var planningBlokken = capaciteitBlokDao.leesCapaciteitBlokken(planningScreeningsEenheid, PlanningConstanten.prognoseVanafDatum, null);
		planningBlokken.forEach(blok ->
		{
			planningScreeningsEenheid.getBlokSet().add(blok);

			var planningDag = blok.getDag();
			planningDag.getBlokSet().add(blok);

			PlanningBlokIndex.put(blok);

			var wijzigingenRoute = PlanningWijzigingen.getWijzigingenRoute(planningScreeningsEenheid);
			wijzigingenRoute.getBlokSet().add(blok);
			wijzigingenRoute.getDagSet().add(planningDag);
			wijzigingenRoute.getWeekSet().add(planningDag.getWeek());
		});

		LOG.info("{} blokken gelezen voor se: {}", planningBlokken.size(), planningScreeningsEenheid.getId());
	}

	private void leesStandplaatsen(PlanningScreeningsOrganisatie planningScreeningsOrganisatie)
	{
		LOG.info("leesStandplaatsen so: {}", planningScreeningsOrganisatie.getId());

		var actieveStandplaatsIds = planningStandplaatsDao.findActieveStandplaatsIds(planningScreeningsOrganisatie);

		for (var standplaatsId : actieveStandplaatsIds)
		{
			var planningStandplaats = new PlanningStandplaats(standplaatsId);
			planningStandplaats.setScreeningsOrganisatie(planningScreeningsOrganisatie);
			planningScreeningsOrganisatie.getStandplaatsSet().add(planningStandplaats);
			PlanningStandplaatsIndex.put(planningStandplaats);

			leesPostcodeReeksen(planningStandplaats);

			PlanningWijzigingen.getStandplaatsSet().add(planningStandplaats);
		}

		LOG.info("{} standplaatsen gelezen voor so: {}", actieveStandplaatsIds.size(), planningScreeningsOrganisatie.getId());
	}

	private void leesPostcodeReeksen(PlanningStandplaats planningStandplaats)
	{
		LOG.info("leesPostcodeReeksen standplaats: {}", planningStandplaats.getId());

		var persistentPostcodeReeksen = planningPostcodeReeksRepository.findAllByStandplaatsId(planningStandplaats.getId());

		for (var persistentPostcodeReeks : persistentPostcodeReeksen)
		{
			var planningPostcodeReeks = new PlanningPostcodeReeks(persistentPostcodeReeks.getId(), persistentPostcodeReeks.getVanPostcode(),
				persistentPostcodeReeks.getTotPostcode());
			planningPostcodeReeks.setStandplaats(planningStandplaats);
			planningStandplaats.getPostcodeReeksSet().add(planningPostcodeReeks);

			PlanningPostcodeReeksIndex.put(planningPostcodeReeks);

			PlanningWijzigingen.getPostcodeReeksSet().add(planningPostcodeReeks);
		}

		LOG.info("{} postcodereeksen gelezen voor standplaats: {}", persistentPostcodeReeksen.size(), planningStandplaats.getId());
	}

	private void leesStandplaatsRonden(Set<Long> teLezenStandplaatsRondeIds)
	{
		LOG.info("leesStandplaatsRonden");

		if (!teLezenStandplaatsRondeIds.isEmpty())
		{
			var standplaatsRondeDtos = planningStandplaatsRondeDao.standplaatsRondenVoorConceptmodel(teLezenStandplaatsRondeIds);

			for (var standplaatsRondeDto : standplaatsRondeDtos)
			{
				var planningAchtervangStandplaats = Optional.ofNullable(standplaatsRondeDto.getAchtervangStandplaatsId()).map(PlanningStandplaatsIndex::get).orElse(null);

				var planningMinderValideUitwijkStandplaats = Optional.ofNullable(standplaatsRondeDto.getMinderValideUitwijkStandplaatsId()).map(PlanningStandplaatsIndex::get)
					.orElse(null);

				var planningStandplaatsRonde = new PlanningStandplaatsRonde(standplaatsRondeDto.getId(), standplaatsRondeDto.getAfspraakDrempel(),
					standplaatsRondeDto.getInterval(),
					planningAchtervangStandplaats, planningMinderValideUitwijkStandplaats, standplaatsRondeDto.getAchtervangToegepast(),
					DateUtil.toLocalDate(standplaatsRondeDto.getMinderValideUitnodigenVanaf()), standplaatsRondeDto.getExtraMinderValideCapaciteitUitgenodigd());

				planningStandplaatsRonde.setStandplaats(PlanningStandplaatsIndex.get(standplaatsRondeDto.getStandplaatsId()));

				PlanningStandplaatsRondeIndex.put(planningStandplaatsRonde);
			}

			leesScreeningOrganisatiesMetCapaciteitBeschikbaarVoor(teLezenStandplaatsRondeIds);

			LOG.info("{} standplaatsronden gelezen", standplaatsRondeDtos.size());
		}
	}

	private void leesScreeningOrganisatiesMetCapaciteitBeschikbaarVoor(Set<Long> teLezenStandplaatsRondeIdSet)
	{
		LOG.info("leesScreeningOrganisatiesMetCapaciteitBeschikbaarVoor");
		var capaciteitBeschikbaarVoorSoDtos = planningStandplaatsRondeDao.capaciteitBeschikbaarVoorScreeningOrganisaties(teLezenStandplaatsRondeIdSet);
		for (var capaciteitBeschikbaarVoorSoDto : capaciteitBeschikbaarVoorSoDtos)
		{
			var planningStandplaatsRonde = PlanningStandplaatsRondeIndex.get(capaciteitBeschikbaarVoorSoDto.getStandplaatsRondeId());
			planningStandplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor().add(PlanningScreeningsOrganisatieIndex.get(capaciteitBeschikbaarVoorSoDto.getScreeningOrganisatieId()));
		}
	}

	private void leesStandplaatsPerioden(Set<Long> teLezenStandplaatsPeriodeIds)
	{
		LOG.info("leesStandplaatsPeriode");

		if (!teLezenStandplaatsPeriodeIds.isEmpty())
		{
			var persistentStandplaatsPeriodes = planningStandplaatsPeriodeRepository.findAllById(teLezenStandplaatsPeriodeIds);

			for (var persistentStandplaatsPeriode : persistentStandplaatsPeriodes)
			{
				voegStandplaatsPeriodeToeAanConceptmodel(persistentStandplaatsPeriode);
			}

			LOG.info("{} standplaatsperioden gelezen", persistentStandplaatsPeriodes.size());
		}
	}

	private void voegStandplaatsPeriodeToeAanConceptmodel(MammaStandplaatsPeriode persistentStandplaatsPeriode)
	{
		var planningStandplaatsPeriode = new PlanningStandplaatsPeriode(persistentStandplaatsPeriode.getId(), persistentStandplaatsPeriode.getScreeningsEenheidVolgNr(),
			persistentStandplaatsPeriode.getStandplaatsRondeVolgNr(), DateUtil.toLocalDate(persistentStandplaatsPeriode.getVanaf()), persistentStandplaatsPeriode.getPrognose(),
			DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet()));

		var planningScreeningsEenheid = PlanningScreeningsEenheidIndex.get(persistentStandplaatsPeriode.getScreeningsEenheid().getId());
		planningScreeningsEenheid.getStandplaatsPeriodeNavigableSet().add(planningStandplaatsPeriode);
		planningStandplaatsPeriode.setScreeningsEenheid(planningScreeningsEenheid);

		var currVolgNrOffset = planningScreeningsEenheid.getVolgNrOffset();
		if (currVolgNrOffset > planningStandplaatsPeriode.getScreeningsEenheidVolgNr())
		{
			currVolgNrOffset = planningStandplaatsPeriode.getScreeningsEenheidVolgNr();
		}
		planningScreeningsEenheid.setVolgNrOffset(currVolgNrOffset);

		var planningStandplaatsRonde = PlanningStandplaatsRondeIndex.get(persistentStandplaatsPeriode.getStandplaatsRonde().getId());
		planningStandplaatsRonde.getStandplaatsPeriodeNavigableSet().add(planningStandplaatsPeriode);
		planningStandplaatsPeriode.setStandplaatsRonde(planningStandplaatsRonde);

		planningStandplaatsRonde.getStandplaats().getStandplaatsRondeNavigableSet().add(planningStandplaatsRonde);

		PlanningStandplaatsPeriodeIndex.put(planningStandplaatsPeriode);

		PlanningWijzigingen.getWijzigingenRoute(planningScreeningsEenheid).setVanafStandplaatsPeriode(planningStandplaatsPeriode);
	}

	private void leesTehuizen()
	{
		LOG.info("leesTehuizen");

		var tehuisDtos = planningTehuisDao.actieveTehuizen();

		for (var tehuisDto : tehuisDtos)
		{
			var planningTehuis = new PlanningTehuis(tehuisDto.getId(), PlanningStandplaatsIndex.get(tehuisDto.getStandplaatsId()));
			PlanningScreeningsOrganisatieIndex.get(tehuisDto.getScreeningOrganisatieId()).getTehuisSet().add(planningTehuis);
			PlanningTehuisIndex.put(planningTehuis);
			PlanningWijzigingen.getTehuisSet().add(planningTehuis);
		}

		LOG.info("{} tehuizen gelezen", tehuisDtos.size());
	}

	private void leesClienten()
	{
		LOG.info("leesClienten: start query vorigeScreeningRondeDatums");
		var vorigeScreeningRondeDatumPerDossier = planningScreeningRondeDao.vorigeScreeningRondeDatumPerDossier();
		LOG.info("leesClienten: {} vorigeScreeningRondeDatums gelezen", vorigeScreeningRondeDatumPerDossier.size());

		var clientDtos = planningClientDao.clientenVoorConceptmodel();
		LOG.info("leesClienten: einde query ophalen clienten");

		for (var clientDto : clientDtos)
		{
			leesClientDtoInConceptmodel(clientDto, vorigeScreeningRondeDatumPerDossier);
		}

		LOG.info("{} clienten gelezen in conceptmodel", clientDtos.size());
	}

	private void leesClientDtoInConceptmodel(ClientDaoDto clientDto, Map<Long, LocalDate> vorigeScreeningRondeDatumPerDossier)
	{
		var planningClient = registreerNieuweClientInConceptmodel(clientDto);

		var standplaatsVanPostcodeOfTehuis = bepaalEnRegistreerStandplaatsVanPostcodeOfTehuis(planningClient);

		var oorspronkelijkeStandplaatsRonde =
			clientDto.getOorspronkelijkeStandplaatsRondeId() != null ? PlanningStandplaatsRondeIndex.get(clientDto.getOorspronkelijkeStandplaatsRondeId()) : null;

		registreerUitnodigingStatusVanClient(planningClient, clientDto, standplaatsVanPostcodeOfTehuis, oorspronkelijkeStandplaatsRonde, vorigeScreeningRondeDatumPerDossier);

		koppelAfspraakOfUitstelAanStandplaatsEnRegistreerVerhuizingTussenStandplaatsen(planningClient, standplaatsVanPostcodeOfTehuis, oorspronkelijkeStandplaatsRonde);
	}

	@NotNull
	private PlanningClient registreerNieuweClientInConceptmodel(ClientDaoDto clientDto)
	{
		var uitstelStandplaats = bepaalUitstelStandplaats(clientDto);
		var afspraakStandplaats = bepaalAfspraakStandplaats(clientDto);

		if (uitstelStandplaats != null && afspraakStandplaats != null)
		{
			if (clientDto.getUitstelUitnodigingId() == null)
			{
				afspraakStandplaats = null;
			}
			else
			{
				uitstelStandplaats = null;
			}
		}

		var planningClient = new PlanningClient(
			clientDto.getId(),
			DateUtil.toLocalDate(clientDto.getGeboortedatum()),
			postcodeVoorClientIndienNietInTehuis(clientDto),
			clientDto.getEersteOnderzoek(),
			clientDto.getDoelgroep(),
			clientDto.getDeelnamekans(),
			clientDto.getVoorgaandeScreeningRondes() != null && clientDto.getVoorgaandeScreeningRondes() > 0,
			PlanningScreeningsOrganisatieIndex.get(clientDto.getScreeningOrgansatieId()),
			uitstelStandplaats,
			uitstelStandplaats == null ? null : DateUtil.toLocalDate(clientDto.getUitstelStreefDatum()),
			uitstelStandplaats == null ? null : clientDto.getUitstelUitnodigingId() != null,
			uitstelStandplaats == null ? null : clientDto.getUitstelReden(),
			afspraakStandplaats,
			clientDto.getTehuisId() != null ? PlanningTehuisIndex.get(clientDto.getTehuisId()) : null,
			DateUtil.toLocalDate(clientDto.getScreeningRondeCreatieDatum()),
			DateUtil.toLocalDate(clientDto.getLaatsteMammografieAfgerondOp()),
			clientDto.getUitnodigingsIntervalType(),
			DateUtil.toLocalDate(clientDto.getLaatsteUitnodigingDatum()));

		planningClient.setNoShow(afspraakService.isNoShow(clientDto.getAfspraakStatus(), DateUtil.toLocalDateTime(clientDto.getAfspraakMoment())));

		PlanningClientIndex.put(planningClient);
		PlanningWijzigingen.getClientSet().add(planningClient);

		return planningClient;
	}

	@Nullable
	private PlanningStandplaats bepaalUitstelStandplaats(ClientDaoDto clientDto)
	{
		if (clientDto.getOorspronkelijkeStandplaatsRondeId() != null
			&& clientDto.getUitstelStandplaatsId() != null
			&& (clientDto.getUitstelUitnodigingId() == null || PlanningStandplaatsRondeIndex.get(clientDto.getUitnodigingStandplaatsRondeId()) != null))
		{
			return PlanningStandplaatsIndex.get(clientDto.getUitstelStandplaatsId());
		}
		return null;
	}

	@Nullable
	private PlanningStandplaats bepaalAfspraakStandplaats(ClientDaoDto clientDto)
	{
		if (clientDto.getAfspraakStandplaatsRondeId() != null
			&& clientDto.getAfspraakAfgezegdOp() == null
			&& PlanningStandplaatsRondeIndex.get(clientDto.getAfspraakStandplaatsRondeId()) != null)
		{
			return PlanningStandplaatsRondeIndex.get(clientDto.getAfspraakStandplaatsRondeId()).getStandplaats();
		}
		return null;
	}

	@Nullable
	private String postcodeVoorClientIndienNietInTehuis(ClientDaoDto clientDto)
	{
		if (clientDto.getTehuisId() == null)
		{
			return clientDto.getTijdelijkGbaPostcode() != null ? clientDto.getTijdelijkGbaPostcode() : clientDto.getPostcode();
		}
		return null;
	}

	@Nullable
	private PlanningStandplaats bepaalEnRegistreerStandplaatsVanPostcodeOfTehuis(PlanningClient planningClient)
	{
		PlanningStandplaats standplaatsVanPostcodeOfTehuis = null;
		var planningTehuis = planningClient.getTehuis();
		if (planningTehuis == null)
		{
			var planningPostcodeReeks = PlanningPostcodeReeksIndex.get(planningClient.getPostcode());
			if (planningPostcodeReeks != null)
			{
				standplaatsVanPostcodeOfTehuis = planningPostcodeReeks.getStandplaats();
				registreerPostcodeReeksRegioInConceptmodel(planningClient, planningPostcodeReeks);
			}
		}
		else
		{
			standplaatsVanPostcodeOfTehuis = planningTehuis.getStandplaats();
			standplaatsVanPostcodeOfTehuis.getTehuisSet().add(planningTehuis);
			planningTehuis.getClientSet().add(planningClient);
		}

		if (standplaatsVanPostcodeOfTehuis != null)
		{
			standplaatsVanPostcodeOfTehuis.getScreeningsOrganisatie().getClientList().add(planningClient);
			PlanningClientFactorTypeIndex.put(planningClient);
		}
		else
		{
			PlanningClientZonderPostcodeReeksIndex.putClient(planningClient);
		}

		return standplaatsVanPostcodeOfTehuis;
	}

	private void registreerPostcodeReeksRegioInConceptmodel(PlanningClient planningClient, PlanningPostcodeReeks planningPostcodeReeks)
	{
		var postcodeReeksRegio = planningPostcodeReeks.getPostcodeReeksRegio(planningClient);
		postcodeReeksRegio.getClientSet().add(planningClient);
		PlanningWijzigingen.getPostcodeReeksRegioSet().add(postcodeReeksRegio);
	}

	private void registreerUitnodigingStatusVanClient(PlanningClient planningClient, ClientDaoDto clientDto, PlanningStandplaats standplaatsVanPostcodeOfTehuis,
		PlanningStandplaatsRonde oorspronkelijkeStandplaatsRonde, Map<Long, LocalDate> vorigeScreeningRondeDatumPerDossier)
	{
		if (standplaatsVanPostcodeOfTehuis != null && !standplaatsVanPostcodeOfTehuis.getStandplaatsRondeNavigableSet().isEmpty())
		{
			if (isClientAlUitgenodigd(standplaatsVanPostcodeOfTehuis, oorspronkelijkeStandplaatsRonde))
			{
				planningClient.setUitgenodigdHuidigeStandplaatsRonde(true);
				planningClient.setUitgenodigdHuidigeStandplaatsRondeIsGeforceerd(clientDto.getScreeningRondeIsGeforceerd());
				if (vorigeScreeningRondeDatumPerDossier.containsKey(clientDto.getDossierId()))
				{
					planningClient.setVorigeScreeningRondeCreatieDatum(vorigeScreeningRondeDatumPerDossier.get(clientDto.getDossierId()));
				}
			}
			else
			{
				planningClient.setVorigeScreeningRondeCreatieDatum(DateUtil.toLocalDate(clientDto.getScreeningRondeCreatieDatum()));
			}
		}
	}

	private boolean isClientAlUitgenodigd(PlanningStandplaats standplaatsVanPostcodeOfTehuis, PlanningStandplaatsRonde oorspronkelijkeStandplaatsRonde)
	{
		var eersteStandplaatsRondeVoorPostcodeClient = standplaatsVanPostcodeOfTehuis.getStandplaatsRondeNavigableSet().first();
		return eersteStandplaatsRondeVoorPostcodeClient.equals(oorspronkelijkeStandplaatsRonde);
	}

	private void koppelAfspraakOfUitstelAanStandplaatsEnRegistreerVerhuizingTussenStandplaatsen(PlanningClient planningClient, PlanningStandplaats standplaatsVanPostcodeOfTehuis,
		PlanningStandplaatsRonde oorspronkelijkeStandplaatsRonde)
	{
		PlanningStandplaats transportStandplaats = null;
		var uitstelStandplaats = planningClient.getUitstelStandplaats();
		var afspraakStandplaats = planningClient.getAfspraakStandplaats();
		if (afspraakStandplaats != null)
		{
			afspraakStandplaats.getAfspraakSet().add(planningClient);
			if (!afspraakStandplaats.equals(standplaatsVanPostcodeOfTehuis))
			{
				transportStandplaats = afspraakStandplaats;
			}
		}
		else if (uitstelStandplaats != null)
		{
			uitstelStandplaats.getUitstelSet().add(planningClient);
			if (!uitstelStandplaats.equals(standplaatsVanPostcodeOfTehuis))
			{
				transportStandplaats = uitstelStandplaats;
			}
		}

		if (transportStandplaats == null && oorspronkelijkeStandplaatsRonde != null)
		{
			var screeningRondeStandplaats = oorspronkelijkeStandplaatsRonde.getStandplaats();
			if (!screeningRondeStandplaats.equals(standplaatsVanPostcodeOfTehuis))
			{
				transportStandplaats = screeningRondeStandplaats;
				oorspronkelijkeStandplaatsRonde.getScreeningRondeTransportSet().add(planningClient);
			}
		}

		if (transportStandplaats != null)
		{
			if (standplaatsVanPostcodeOfTehuis != null)
			{
				standplaatsVanPostcodeOfTehuis.getTransportVanSet().add(planningClient);
			}
			transportStandplaats.getTransportNaarSet().add(planningClient);
		}
	}

	private void leesBlokkades()
	{
		LOG.info("leesBlokkades");

		var blokkadeDtos = planningBlokkadeDao.actieveBlokkadesVoorConceptmodel();

		for (var blokkadeDto : blokkadeDtos)
		{
			var blokkade = maakPlanningBlokkade(blokkadeDto);
			PlanningBlokkadeIndex.put(blokkade);

		}

		LOG.info(blokkadeDtos.size() + " blokkades gelezen");
	}

	@NotNull
	private static PlanningBlokkade maakPlanningBlokkade(BlokkadeDaoDto blokkadeDto)
	{
		PlanningStandplaats planningStandplaats = null;
		PlanningScreeningsEenheid planningScreeningsEenheid = null;
		PlanningScreeningsOrganisatie planningScreeningsOrganisatie = null;

		switch (blokkadeDto.getType())
		{
		case SCREENINGS_ORGANISATIE:
			planningScreeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(blokkadeDto.getScreeningOrganisatieId());
			break;
		case SCREENINGS_EENHEID:
			planningScreeningsEenheid = PlanningScreeningsEenheidIndex.get(blokkadeDto.getScreeningsEenheidId());
			break;
		case STANDPLAATS:
			planningStandplaats = PlanningStandplaatsIndex.get(blokkadeDto.getStandplaatsId());
			break;
		}

		return new PlanningBlokkade(blokkadeDto.getId(), blokkadeDto.getType(), planningStandplaats, planningScreeningsEenheid, planningScreeningsOrganisatie,
			DateUtil.toLocalDate(blokkadeDto.getVanaf()), DateUtil.toLocalDate(blokkadeDto.getTotEnMet()));
	}

	private void leesGebruikteCapaciteit()
	{
		LOG.info("leesGebruikteCapaciteit");

		var teLezenStandplaatsPeriodeIdsVanAlleScreeningOrganisaties = teLezenStandplaatsPeriodeIdsPerScreeningsOrganisatieId.values().stream().flatMap(Set::stream)
			.collect(Collectors.toSet());

		if (!teLezenStandplaatsPeriodeIdsVanAlleScreeningOrganisaties.isEmpty())
		{
			var afspraakDtos = planningAfspraakDao.afsprakenMetGebruikteCapaciteit(teLezenStandplaatsPeriodeIdsVanAlleScreeningOrganisaties);
			for (var afspraakDto : afspraakDtos)
			{
				var planningClient = PlanningClientIndex.get(afspraakDto.getClientId());
				if (planningClient != null)
				{
					var screeningsEenheid = PlanningScreeningsEenheidIndex.get(afspraakDto.getScreeningsEenheidId());
					var dag = screeningsEenheid.getDagNavigableMap().get(DateUtil.toLocalDate(afspraakDto.getAfspraakMoment()));
					dag.getBeschikbaar().add(planningClient.getGebruikteCapaciteit(screeningsEenheid.getScreeningsOrganisatie()), planningClient.getBlokType());
				}
			}
		}

		LOG.info("leesGebruikteCapaciteit gelezen voor {} standplaatsPeriodes", teLezenStandplaatsPeriodeIdsVanAlleScreeningOrganisaties.size());
	}
}
