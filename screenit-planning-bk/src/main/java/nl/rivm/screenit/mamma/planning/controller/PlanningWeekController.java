package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.sql.Date;
import java.util.HashSet;
import java.util.Set;

import nl.rivm.screenit.dto.mamma.planning.PlanningDagDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningHerhalenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_WEEK)
public class PlanningWeekController
{
	@Autowired
	private PlanningConceptService conceptService;

	@RequestMapping(value = "/{screeningsEenheidId}/{maandag}", method = RequestMethod.GET)
	@ResponseBody
	public PlanningWeekDto getBeschikbareCapaciteit(@PathVariable Long screeningsEenheidId, @PathVariable Long maandag)
	{
		PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get(screeningsEenheidId);
		PlanningWeek week = screeningsEenheid.getWeek(new Date(maandag).toLocalDate());

		PlanningWeekDto weekDto = new PlanningWeekDto();
		Set<PlanningBlokkade> blokkadeSet = new HashSet<>();
		Set<PlanningStandplaatsPeriode> standplaatsPeriodeSet = new HashSet<>();

		for (PlanningDag dag : week.getDagList())
		{
			PlanningDagDto dagDto = new PlanningDagDto();
			dagDto.datum = dag.getDatum();
			dagDto.totaalAantalOnderzoeken = dag.getBeschikbaar().getTotaal().longValue();

			for (PlanningBlok blok : dag.getBlokSet())
			{
				weekDto.blokken.add(PlanningMapper.from(blok));
			}

			for (PlanningBlokkade blokkade : dag.getBlokkadeSet())
			{
				blokkadeSet.add(blokkade);
			}
			if (dag.getStandplaatsPeriode() != null && dag.getStandplaatsPeriode().getStandplaatsRonde() != null
				&& dag.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats() != null)
			{
				standplaatsPeriodeSet.add(dag.getStandplaatsPeriode());
			}
			weekDto.dagen.add(dagDto);
		}

		blokkadeSet.forEach(blokkade -> weekDto.blokkadesIds.add(blokkade.getId()));
		standplaatsPeriodeSet.forEach(standplaatsPeriode -> weekDto.standplaatsPeriodes.add(PlanningMapper.from(standplaatsPeriode)));

		return weekDto;
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void herhalen(@RequestBody PlanningHerhalenDto herhalenDto)
	{
		PlanningScreeningsEenheid screeningsEenheidVan = PlanningScreeningsEenheidIndex.get(herhalenDto.screeningsEenheidIdVan);
		conceptService.herhalen(screeningsEenheidVan,
			PlanningScreeningsEenheidIndex.get(herhalenDto.screeningsEenheidIdNaar),
			screeningsEenheidVan.getWeek(herhalenDto.teHerhalenWeek),
			herhalenDto.herhalenVanaf,
			herhalenDto.herhalenTotEnMet);

		PlanningDoorrekenenManager.run();
	}

	@RequestMapping(value = "/plannenTotEnMetDatum", method = RequestMethod.GET)
	public java.util.Date getPlannenTotEnMetDatum()
	{
		return DateUtil.toUtilDate(PlanningConstanten.plannenTotEnMetDatum);
	}

}
