package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.dto.ErrorDto;
import nl.rivm.screenit.mamma.se.security.SEAccountResolverDelegate;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.PassantInschrijvenValidatorService;
import nl.rivm.screenit.mamma.se.service.dtomapper.AfspraakDtoMapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("passanten")
public class PassantZoekenController extends AuthorizedController
{
	@Autowired
	private ClientService clientService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	@Autowired
	private PassantInschrijvenValidatorService passantInschrijvenValidatorService;

	private AfspraakDtoMapper dtoMapper = new AfspraakDtoMapper();

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity getClientByBsnEnControleerGeboortedatum(@RequestParam("bsn") String bsn,
		@RequestParam("geboortedatum") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate geboortedatum, HttpServletRequest request)
	{
		if (!isAuthorized(request, Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN))
		{
			return createUnauthorizedResponse();
		}
		SEAccountResolverDelegate.setInstellingGebruiker(getInstellingGebruiker(request));

		Client client = clientService.getClientByBsn(bsn);
		if (client != null && client.getMammaDossier() != null)
		{
			if (DateUtil.isGeboortedatumGelijk(geboortedatum, client))
			{
				MammaScreeningsEenheid screeningsEenheid = getScreeningsEenheid(request);
				if (passantInschrijvenValidatorService.isGeldigPassantScenario(client, currentDateSupplier.getLocalDate(), screeningsEenheid))
				{
					return ResponseEntity.ok(dtoMapper.createPassantDto(client.getMammaDossier()));
				}
				else
				{
					MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
					if (laatsteScreeningRonde != null)
					{
						MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
						if (laatsteAfspraak != null
							&& DateUtil.isZelfdeDag(currentDateSupplier.getLocalDate(), laatsteAfspraak.getVanaf()) &&
							laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().equals(screeningsEenheid))
						{
							return ResponseEntity.status(HttpStatus.NOT_FOUND)
								.body(new ErrorDto("Cliënt heeft al een afspraak voor vandaag op deze SE"));
						}
					}
					return ResponseEntity.status(HttpStatus.NOT_FOUND)
						.body(new ErrorDto("Er kan geen afspraak gemaakt worden voor de cliënt, de client kan voor meer informatie contact opnemen met de infolijn."));
				}
			}
		}

		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new ErrorDto("Cliënt is niet gevonden."));
	}
}
