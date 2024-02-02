package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.mamma.planning.index.PlanningClientIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_CLIENT)
public class PlanningClientController
{
	private final HibernateService hibernateService;

	public PlanningClientController(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningVerzetClientenDto verzetClientenDto)
	{
		MammaStandplaatsPeriode mammaStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, verzetClientenDto.verzetStandplaatsPeriodeId);
		PlanningStandplaats nieuweAfspraakStandplaats = PlanningStandplaatsIndex.get(mammaStandplaatsPeriode.getStandplaatsRonde().getStandplaats().getId());

		verzetClientenDto.clientIdSet.forEach(clientId ->
		{

			PlanningClient client = PlanningClientIndex.get(clientId);
			PlanningStandplaats standplaats = client.getStandplaats();

			if (standplaats != null)
			{
				standplaats.getTransportVanSet().remove(client);
				PlanningWijzigingen.bepaalWijzigingen(standplaats);
			}

			PlanningStandplaats afspraakStandplaats = client.getAfspraakStandplaats();
			if (afspraakStandplaats != null)
			{
				afspraakStandplaats.getTransportNaarSet().remove(client);
				PlanningWijzigingen.bepaalWijzigingen(afspraakStandplaats);
			}

			PlanningStandplaats uitstelStandplaats = client.getUitstelStandplaats();
			if (uitstelStandplaats != null)
			{
				uitstelStandplaats.getTransportNaarSet().remove(client);
				PlanningWijzigingen.bepaalWijzigingen(uitstelStandplaats);
			}

			if (!nieuweAfspraakStandplaats.equals(standplaats))
			{
				if (standplaats != null)
				{
					standplaats.getTransportVanSet().add(client);
					PlanningWijzigingen.bepaalWijzigingen(standplaats);
				}
				nieuweAfspraakStandplaats.getTransportNaarSet().add(client);
				PlanningWijzigingen.bepaalWijzigingen(nieuweAfspraakStandplaats);
			}

			client.setAfspraakStandplaats(nieuweAfspraakStandplaats);
		});

		PlanningDoorrekenenManager.run();
	}
}
