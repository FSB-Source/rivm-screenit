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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.dto.mamma.planning.PlanningPostcodeReeksDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.mamma.planning.index.PlanningClientZonderPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_POSTCODEREEKS)
public class PlanningPostcodeReeksController
{
	@RequestMapping(method = RequestMethod.POST)
	public void post(@RequestBody PlanningPostcodeReeksDto postcodeReeksDto)
	{
		PlanningStandplaats standplaats = PlanningStandplaatsIndex.get(postcodeReeksDto.standplaatsId);

		PlanningPostcodeReeks postcodeReeks = new PlanningPostcodeReeks(postcodeReeksDto.id, postcodeReeksDto.vanPostcode, postcodeReeksDto.totPostcode);
		postcodeReeks.setStandplaats(standplaats);
		standplaats.getPostcodeReeksSet().add(postcodeReeks);

		PlanningPostcodeReeksIndex.put(postcodeReeks);

		clientenZonderPostcodeReeksToevoegen(postcodeReeks);

		PlanningWijzigingen.getPostcodeReeksSet().add(postcodeReeks);
		PlanningWijzigingen.bepaalWijzigingen(standplaats);

		PlanningDoorrekenenManager.run();
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningPostcodeReeksDto postcodeReeksDto)
	{
		PlanningPostcodeReeks postcodeReeks = PlanningPostcodeReeksIndex.get(postcodeReeksDto.id);
		Set<String> oudePostcodeSet = postcodeReeks.getPostcodeSet();
		postcodeReeks.setPostcodeReeks(postcodeReeksDto.vanPostcode, postcodeReeksDto.totPostcode);
		PlanningPostcodeReeksIndex.update(postcodeReeks, oudePostcodeSet);

		List<String> removedPlanningPostcodeReeksRegioList = new ArrayList<>();
		for (PlanningPostcodeReeksRegio postcodeReeksRegio : postcodeReeks.getPostcodeReeksRegios())
		{
			Set<PlanningClient> postcodeReeksRegioClientSet = postcodeReeksRegio.getClientSet();
			List<PlanningClient> removedClientList = new ArrayList<>();
			for (PlanningClient client : postcodeReeksRegioClientSet)
			{
				if (!postcodeReeks.inPostcodeReeks(client))
				{
					PlanningClientZonderPostcodeReeksIndex.putClient(client);
					removedClientList.add(client);
				}
			}
			postcodeReeksRegioClientSet.removeAll(removedClientList);
			if (postcodeReeksRegio.getClientSet().isEmpty())
			{
				removedPlanningPostcodeReeksRegioList.add(postcodeReeksRegio.getCijfer());
			}
		}
		postcodeReeks.removePostcodeReeksRegios(removedPlanningPostcodeReeksRegioList);

		clientenZonderPostcodeReeksToevoegen(postcodeReeks);

		PlanningStandplaats oudeStandplaats = postcodeReeks.getStandplaats();
		PlanningStandplaats nieuweStandplaats = PlanningStandplaatsIndex.get(postcodeReeksDto.standplaatsId);
		if (!nieuweStandplaats.equals(oudeStandplaats))
		{
			oudeStandplaats.getPostcodeReeksSet().remove(postcodeReeks);

			nieuweStandplaats.getPostcodeReeksSet().add(postcodeReeks);
			postcodeReeks.setStandplaats(nieuweStandplaats);

			PlanningWijzigingen.bepaalWijzigingen(oudeStandplaats);
		}
		PlanningWijzigingen.getPostcodeReeksSet().add(postcodeReeks);
		PlanningWijzigingen.bepaalWijzigingen(nieuweStandplaats);

		PlanningDoorrekenenManager.run();
	}

	private void clientenZonderPostcodeReeksToevoegen(PlanningPostcodeReeks postcodeReeks)
	{
		for (PlanningClient client : PlanningClientZonderPostcodeReeksIndex.getClienten(postcodeReeks))
		{
			PlanningPostcodeReeksRegio postcodeReeksRegio = postcodeReeks.getPostcodeReeksRegio(client);
			postcodeReeksRegio.getClientSet().add(client);

			PlanningClientZonderPostcodeReeksIndex.removeClient(client);

			PlanningWijzigingen.getPostcodeReeksRegioSet().add(postcodeReeksRegio);
		}
	}

	@RequestMapping(value = "/{postcodeReeksId}", method = RequestMethod.DELETE)
	public void delete(@PathVariable Long postcodeReeksId)
	{
		PlanningPostcodeReeks postcodeReeks = PlanningPostcodeReeksIndex.get(postcodeReeksId);

		for (PlanningPostcodeReeksRegio postcodeReeksRegio : postcodeReeks.getPostcodeReeksRegios())
		{
			for (PlanningClient client : postcodeReeksRegio.getClientSet())
			{
				PlanningClientZonderPostcodeReeksIndex.putClient(client);
			}
		}
		PlanningPostcodeReeksIndex.remove(postcodeReeks);
		postcodeReeks.getStandplaats().getPostcodeReeksSet().remove(postcodeReeks);

		PlanningWijzigingen.bepaalWijzigingen(postcodeReeks.getStandplaats());

		PlanningDoorrekenenManager.run();
	}
}
