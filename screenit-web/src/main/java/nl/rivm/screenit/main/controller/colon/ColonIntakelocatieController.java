package nl.rivm.screenit.main.controller.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.colon.ColonIntakelocatieMapper;
import nl.rivm.screenit.model.colon.dto.ColonIntakelocatieDto;
import nl.rivm.screenit.model.colon.dto.ColonSignaleringstermijnDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonRoosterService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@RestController
@RequestMapping("/api/colon/intakelocatie")
@AllArgsConstructor
public class ColonIntakelocatieController
{
	private final ColonIntakelocatieMapper intakelocatieMapper;

	private final ColonRoosterService roosterService;

	@GetMapping()
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ColonIntakelocatieDto getIntakelocatie()
	{
		var intakelocatie = ScreenitSession.get().getColoscopieCentrum();
		return intakelocatieMapper.intakelocatieToDto(intakelocatie);
	}

	@GetMapping("/signaleringstermijn")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonSignaleringstermijnDto> getSignaleringstermijn()
	{
		var signaleringstermijnDto = new ColonSignaleringstermijnDto();

		var signaleringstermijnTekst = roosterService.getSignaleringstermijnTekst();

		signaleringstermijnDto.setTekst(signaleringstermijnTekst);
		signaleringstermijnDto.setSignaleringsTermijnDeadline(roosterService.getSignaleringstermijnDeadline());
		signaleringstermijnDto.setHeeftGeenCapaciteitBinnenSignaleringsTermijn(roosterService.intakelocatieHeeftGeenCapaciteit(ScreenitSession.get().getColoscopieCentrum()));

		return ResponseEntity.ok(signaleringstermijnDto);
	}
}
