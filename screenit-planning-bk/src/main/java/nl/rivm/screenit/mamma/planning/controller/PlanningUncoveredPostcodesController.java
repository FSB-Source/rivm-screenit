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

import java.util.NavigableSet;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.mamma.planning.index.PlanningClientZonderPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_UNCOVEREDPOSTCODES)
public class PlanningUncoveredPostcodesController
{
	@RequestMapping(value = "/{screeningsOrganisatieId}", method = RequestMethod.GET)
	public ResponseEntity<NavigableSet<String>> uncoveredPostcodes(@PathVariable Long screeningsOrganisatieId)
	{
		ResponseEntity<NavigableSet<String>> response = new ResponseEntity<>(
			PlanningClientZonderPostcodeReeksIndex.getPostcodes(PlanningScreeningsOrganisatieIndex.get(screeningsOrganisatieId)),
			HttpStatus.OK);
		return response;
	}

}
