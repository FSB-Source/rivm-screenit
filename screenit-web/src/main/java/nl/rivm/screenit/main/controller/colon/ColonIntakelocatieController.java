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

import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.colon.ColonIntakelocatieMapper;
import nl.rivm.screenit.model.colon.dto.ColonIntakelocatieDto;
import nl.rivm.screenit.model.colon.dto.ColonSignaleringstermijnDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import com.google.common.collect.Range;

@RestController
@RequestMapping("/api/colon/intakelocatie")
@AllArgsConstructor
public class ColonIntakelocatieController
{
	private final ColonIntakelocatieMapper intakelocatieMapper;

	private final ColonIntakelocatieService intakelocatieService;

	private final RoosterService roosterService;

	@GetMapping()
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonIntakelocatieDto> getIntakelocatie()
	{
		var intakelocatie = ScreenitSession.get().getColoscopieCentrum();
		var now = LocalDate.now();
		var startOfYear = now.with(TemporalAdjusters.firstDayOfYear());
		var endOfYear = startOfYear.plusYears(1);

		var periode = Range.closed(DateUtil.toUtilDate(startOfYear), DateUtil.toUtilDate(endOfYear));
		var huidigAantalAfspraakslots = roosterService.getCurrentAantalRoosterBlokken(intakelocatie, periode);
		var response = intakelocatieMapper.intakelocatieToDto(intakelocatie);
		response.setHuidigAantalAfspraakslots(huidigAantalAfspraakslots);
		return ResponseEntity.ok(response);
	}

	@GetMapping("/signaleringstermijn")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonSignaleringstermijnDto> getSignaleringstermijn()
	{
		var signaleringstermijnDto = new ColonSignaleringstermijnDto();

		var signaleringstermijnTekst = intakelocatieService.getSignaleringstermijnTekst();

		signaleringstermijnDto.setTekst(signaleringstermijnTekst);
		signaleringstermijnDto.setSignaleringsTermijnDeadline(intakelocatieService.getSignaleringstermijnDeadline());
		signaleringstermijnDto.setHeeftGeenCapaciteitBinnenSignaleringsTermijn(
			intakelocatieService.intakelocatieHeeftGeenCapaciteit(ScreenitSession.get().getColoscopieCentrum()));

		return ResponseEntity.ok(signaleringstermijnDto);
	}
}
