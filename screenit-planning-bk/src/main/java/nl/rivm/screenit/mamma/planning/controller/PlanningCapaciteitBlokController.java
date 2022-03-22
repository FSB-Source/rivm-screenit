package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.NavigableSet;
import java.util.UUID;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingenRoute;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_CAPACITEITBLOK)
public class PlanningCapaciteitBlokController
{
	private final HibernateService hibernateService;

	private final PlanningConceptOpslaanService conceptOpslaanService;

	public PlanningCapaciteitBlokController(HibernateService hibernateService, PlanningConceptOpslaanService conceptOpslaanService)
	{
		this.hibernateService = hibernateService;
		this.conceptOpslaanService = conceptOpslaanService;
	}

	@RequestMapping(method = RequestMethod.POST)
	public void post(@RequestBody PlanningCapaciteitBlokDto capaciteitBlokDto) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningDag dag = maakNieuwBlok(capaciteitBlokDto);
		bepaalWijzigingen(dag);
		PlanningDoorrekenenManager.run();
	}

	public PlanningDag maakNieuwBlok(PlanningCapaciteitBlokDto capaciteitBlokDto) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get(capaciteitBlokDto.screeningsEenheidId);

		PlanningCapaciteitChangeChecker.magCapaciteitOpslaanVerwijderen(capaciteitBlokDto, true);
		LocalDateTime vanaf = DateUtil.toLocalDateTime(capaciteitBlokDto.vanaf);
		PlanningBlok blok = new PlanningBlok(null, vanaf.toLocalTime(), DateUtil.toLocalTime(capaciteitBlokDto.tot),
			capaciteitBlokDto.aantalOnderzoeken, capaciteitBlokDto.blokType, capaciteitBlokDto.opmerkingen, capaciteitBlokDto.minderValideAfspraakMogelijk);
		blok.setScreeningsEenheid(screeningsEenheid);
		screeningsEenheid.getBlokSet().add(blok);

		PlanningDag dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
		dag.getBlokSet().add(blok);
		blok.setDag(dag);
		PlanningBlokIndex.put(blok);

		PlanningBlokIndex.changed(screeningsEenheid, Collections.singleton(blok));
		PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).getBlokSet().add(blok);
		return dag;
	}

	private void bepaalWijzigingen(PlanningDag dag)
	{
		PlanningScreeningsEenheid screeningsEenheid = dag.getScreeningsEenheid();
		PlanningStandplaatsPeriode standplaatsPeriode = dag.getStandplaatsPeriode();
		PlanningWijzigingenRoute wijzigingenRoute = PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid);
		wijzigingenRoute.getDagSet().add(dag);
		wijzigingenRoute.getWeekSet().add(dag.getWeek());
		if (standplaatsPeriode != null)
		{
			wijzigingenRoute.setVanafStandplaatsPeriode(standplaatsPeriode);
		}
		else
		{

			NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
			if (!standplaatsPeriodeNavigableSet.isEmpty() && standplaatsPeriodeNavigableSet.last().getTotEnMet().isBefore(dag.getDatum()))
			{
				wijzigingenRoute.setVanafStandplaatsPeriode(standplaatsPeriodeNavigableSet.first());
			}
		}

		PlanningWeek herhalingsWeek = screeningsEenheid.getHerhalingsWeek();
		if (herhalingsWeek != null)
		{
			LocalDate nieuweHerhalingsWeek = dag.getDatum().plusWeeks(1).with(DayOfWeek.MONDAY);
			if (herhalingsWeek.getDatum().isBefore(nieuweHerhalingsWeek))
			{
				screeningsEenheid.setHerhalingsWeek(screeningsEenheid.getWeek(nieuweHerhalingsWeek));
			}
		}
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningCapaciteitBlokDto capaciteitBlokDto) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningCapaciteitChangeChecker.magCapaciteitOpslaanVerwijderen(capaciteitBlokDto, true);
		PlanningBlok blok = PlanningBlokIndex.get(capaciteitBlokDto.conceptId);
		PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get(capaciteitBlokDto.screeningsEenheidId);

		assert blok.getScreeningsEenheid().equals(screeningsEenheid);

		PlanningDag dag = blok.getDag();

		bepaalWijzigingen(dag);

		blok.setCapaciteitBlokType(capaciteitBlokDto.blokType);
		blok.setAantalOnderzoeken(capaciteitBlokDto.aantalOnderzoeken);
		LocalDateTime vanaf = DateUtil.toLocalDateTime(capaciteitBlokDto.vanaf);
		blok.setVanaf(vanaf.toLocalTime());
		blok.setTot(DateUtil.toLocalTime(capaciteitBlokDto.tot));
		blok.setMinderValideAfspraakMogelijk(capaciteitBlokDto.minderValideAfspraakMogelijk);

		if (MammaCapaciteitBlokType.GEEN_SCREENING.equals(blok.getCapaciteitBlokType()))
		{
			blok.setOpmerkingen(capaciteitBlokDto.opmerkingen);
		}
		else
		{
			blok.setOpmerkingen(null);
		}

		PlanningDag dagNieuw = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());

		if (!dagNieuw.equals(dag))
		{
			dag.getBlokSet().remove(blok);
			dagNieuw.getBlokSet().add(blok);
			blok.setDag(dagNieuw);
			bepaalWijzigingen(dagNieuw);
		}

		PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).getBlokSet().add(blok);

		PlanningBlokIndex.changed(screeningsEenheid, Collections.singleton(blok));

		PlanningDoorrekenenManager.run();
	}

	@RequestMapping(value = "/{conceptId}", method = RequestMethod.DELETE)
	public void delete(@PathVariable UUID conceptId) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningDag dag = verwijderBlok(conceptId);

		bepaalWijzigingen(dag);

		PlanningDoorrekenenManager.run();
	}

	public PlanningDag verwijderBlok(UUID conceptId) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningBlok blok = PlanningBlokIndex.get(conceptId);
		PlanningCapaciteitChangeChecker.magCapaciteitOpslaanVerwijderen(PlanningMapper.from(blok), false);
		PlanningBlokIndex.deleted(blok.getScreeningsEenheid(), Collections.singleton(blok));

		PlanningDag dag = blok.getDag();

		dag.getBlokSet().remove(blok);
		return dag;
	}

	@RequestMapping(value = "/aantalAfsprakenOpBlok/{conceptId}/{nieuwBlokType}/{vanafTime}/{totTime}/{delete}", method = RequestMethod.GET)
	public Integer getAantalAfspraken(@PathVariable UUID conceptId, @PathVariable MammaCapaciteitBlokType nieuwBlokType, @PathVariable Long vanafTime, @PathVariable Long totTime,
		@PathVariable Boolean delete)
	{
		PlanningBlok blok = PlanningBlokIndex.get(conceptId);

		if (blok != null && blok.getId() != null)
		{
			MammaCapaciteitBlok persistentBlok = hibernateService.get(MammaCapaciteitBlok.class, blok.getId());
			if (persistentBlok != null)
			{
				if (Boolean.TRUE.equals(delete))
				{
					return persistentBlok.getAfspraken().size();
				}
				Date vanaf = new Date(vanafTime);
				Date tot = new Date(totTime);
				return conceptOpslaanService.getAantalAfsprakenTeOntkoppelen(persistentBlok, vanaf, tot, nieuwBlokType);
			}
		}
		return 0;
	}
}
