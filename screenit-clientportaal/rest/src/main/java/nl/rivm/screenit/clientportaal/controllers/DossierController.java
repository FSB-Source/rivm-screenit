package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.cervix.CervixDossierMapper;
import nl.rivm.screenit.clientportaal.mappers.colon.ColonDossierMapper;
import nl.rivm.screenit.clientportaal.mappers.mamma.MammaDossierMapper;
import nl.rivm.screenit.clientportaal.model.cervix.CervixDossierDto;
import nl.rivm.screenit.clientportaal.model.colon.ColonDossierDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaDossierDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.BaseClientGebeurtenisService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("dossier")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class DossierController extends AbstractController
{

	private final BaseClientGebeurtenisService clientGebeurtenisService;

	private final CervixDossierMapper cervixDossierMapper;

	private final ColonDossierMapper colonDossierMapper;

	private final MammaDossierMapper mammaDossierMapper;

	@GetMapping("/mamma")
	public ResponseEntity<MammaDossierDto> getMammaDossier(Authentication authentication)
	{
		Client client = getClient(authentication);
		return ResponseEntity.ok(mammaDossierMapper.mapToDto(client.getMammaDossier(), clientGebeurtenisService.getClientMammaGebeurtenissen(client)));
	}

	@GetMapping("/cervix")
	public ResponseEntity<CervixDossierDto> getCervixDossier(Authentication authentication)
	{
		Client client = getClient(authentication);
		return ResponseEntity.ok(cervixDossierMapper.mapToDto(client.getCervixDossier(), clientGebeurtenisService.getClientCervixGebeurtenissen(client)));
	}

	@GetMapping("/colon")
	public ResponseEntity<ColonDossierDto> getColonDossier(Authentication authentication)
	{
		Client client = getClient(authentication);
		return ResponseEntity.ok(colonDossierMapper.mapToDto(client.getColonDossier(), clientGebeurtenisService.getClientColonGebeurtenissen(client)));
	}

}
