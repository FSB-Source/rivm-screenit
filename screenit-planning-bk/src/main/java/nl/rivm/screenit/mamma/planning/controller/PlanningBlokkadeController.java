package nl.rivm.screenit.mamma.planning.controller;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningBlokkadeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokkadeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_BLOKKADE)
public class PlanningBlokkadeController
{

	@RequestMapping(method = RequestMethod.POST)
	public void post(@RequestBody PlanningBlokkadeDto blokkadeDto)
	{
		PlanningBlokkade blokkade = PlanningMapper.from(blokkadeDto);
		PlanningBlokkadeIndex.put(blokkade);
		bepaalWijzigingen(blokkade);

		PlanningDoorrekenenManager.run();
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningBlokkadeDto blokkadeDto)
	{
		PlanningBlokkade oudeBlokkade = PlanningBlokkadeIndex.get(blokkadeDto.id);
		if (oudeBlokkade != null)
		{
			PlanningBlokkadeIndex.remove(oudeBlokkade);
			bepaalWijzigingen(oudeBlokkade);
		}

		post(blokkadeDto);
	}

	@RequestMapping(value = "/{blokkadeId}", method = RequestMethod.DELETE)
	public void delete(@PathVariable Long blokkadeId)
	{
		PlanningBlokkade blokkade = PlanningBlokkadeIndex.get(blokkadeId);
		if (blokkade != null)
		{
			PlanningBlokkadeIndex.remove(blokkade);
			bepaalWijzigingen(blokkade);
			PlanningDoorrekenenManager.run();
		}
	}

	private void bepaalWijzigingen(PlanningBlokkade blokkade)
	{
		switch (blokkade.getBlokkadeType())
		{
		case SCREENINGS_ORGANISATIE:
			for (PlanningScreeningsEenheid se : blokkade.getScreeningsOrganisatie().getScreeningsEenheidSet())
			{
				bepaalSeWijzigingen(blokkade, se);
			}
			break;
		case SCREENINGS_EENHEID:
			bepaalSeWijzigingen(blokkade, blokkade.getScreeningsEenheid());
			break;
		case STANDPLAATS:
			for (PlanningStandplaatsRonde standplaatsRonde : blokkade.getStandplaats().getStandplaatsRondeNavigableSet())
			{
				for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
				{
					if (!standplaatsPeriode.getVanaf().isAfter(blokkade.getTotEnMet()) && !standplaatsPeriode.getTotEnMet().isBefore(blokkade.getVanaf()))
					{
						PlanningWijzigingen.getWijzigingenRoute(standplaatsPeriode.getScreeningsEenheid()).setVanafStandplaatsPeriode(standplaatsPeriode);
					}
				}
			}
			break;
		}
	}

	private void bepaalSeWijzigingen(PlanningBlokkade blokkade, PlanningScreeningsEenheid se)
	{
		PlanningDag dag = se.getDagNavigableMap().get(blokkade.getVanaf());
		if (dag == null)
		{
			dag = se.getDagNavigableMap().firstEntry().getValue();
		}
		PlanningStandplaatsPeriode periode = dag.getStandplaatsPeriode();
		if (periode == null && CollectionUtils.isNotEmpty(se.getStandplaatsPeriodeNavigableSet()))
		{

			periode = se.getStandplaatsPeriodeNavigableSet().last();
		}
		PlanningWijzigingen.getWijzigingenRoute(se).setVanafStandplaatsPeriode(periode);
	}
}
