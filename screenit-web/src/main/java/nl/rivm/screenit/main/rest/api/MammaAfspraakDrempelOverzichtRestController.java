package nl.rivm.screenit.main.rest.api;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/")
@Slf4j
public class MammaAfspraakDrempelOverzichtRestController
{
	@Autowired
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@RequestMapping(value = "/getAfspraakDrempelOverzichtStandplaats", method = RequestMethod.GET)
	public ResponseEntity<String> getAfspraakDrempelOverzichtStandplaats(@RequestParam(value = "standplaatsId") long standplaatsId)
	{
		if (!isAuthorized())
		{
			return getForbiddenResponse();
		}

		return new ResponseEntity(conceptPlanningsApplicatie.getAfspraakDrempelOverzichtStandplaats(standplaatsId), HttpStatus.OK);
	}

	@RequestMapping(value = "/getAfspraakDrempelOverzichtScreeningsOrganisatie", method = RequestMethod.GET)
	public ResponseEntity<String> getAfspraakDrempelOverzichtScreeningsOrganisatie(@RequestParam(value = "screeningsOrganisatieId") long screeningsOrganisatieId)
	{
		if (!isAuthorized())
		{
			return getForbiddenResponse();
		}

		return new ResponseEntity(conceptPlanningsApplicatie.getAfspraakDrempelOverzichtScreeningsOrganisatie(screeningsOrganisatieId), HttpStatus.OK);
	}

	private static ResponseEntity getForbiddenResponse()
	{
		return new ResponseEntity(HttpStatus.FORBIDDEN);
	}

	private static boolean isAuthorized()
	{
		return ScreenitSession.exists() && ScreenitSession.get().isSignedIn();
	}
}
