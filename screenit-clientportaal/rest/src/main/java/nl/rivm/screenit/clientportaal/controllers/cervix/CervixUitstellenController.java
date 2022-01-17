package nl.rivm.screenit.clientportaal.controllers.cervix;

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

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.exception.NotValidException;
import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstelDto;
import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstellenStatusDto;
import nl.rivm.screenit.clientportaal.services.cervix.CervixUitstellenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("cervix/uitstellen")
@RestController
public class CervixUitstellenController extends AbstractController
{

	@Autowired
	private CervixUitstellenService uitstellenService;

	@GetMapping("huidig")
	public ResponseEntity<CervixUitstelDto> getHuidigeCervixUitstel(Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_UITSTEL))
		{
			return ResponseEntity.ok(uitstellenService.getHuidigeCervixUitstel(client));
		}
		return createForbiddenResponse();
	}

	@GetMapping("status")
	public ResponseEntity<CervixUitstellenStatusDto> getCervixUitstelStatus(Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_UITSTEL))
		{
			return ResponseEntity.ok(uitstellenService.getUitstelStatus(client));
		}
		return createForbiddenResponse();
	}

	@PutMapping("aanvragen")
	public ResponseEntity<CervixUitstelDto> vraagUitstelAan(@RequestBody CervixUitstelDto uitstellenDto, Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_UITSTEL))
		{
			if (uitstellenDto.getUitstellenTotDatum() != null)
			{
				switch (uitstellenDto.getUitstelType())
				{
				case ZWANGERSCHAP:
					uitstellenService.valideerDatumBijZwangerschap(uitstellenDto.getUitstellenTotDatum());
					break;
				case ANDERS:
					uitstellenService.valideerDatumBijAnders(uitstellenDto.getUitstellenTotDatum());
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + uitstellenDto.getUitstelType());
				}

				uitstellenService.vraagUitstelAan(client, uitstellenDto);
				return ResponseEntity.ok(uitstellenDto);
			}
			throw new NotValidException("De uitstellen tot datum is niet gevuld");
		}
		return createForbiddenResponse();
	}
}
