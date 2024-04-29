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

import java.time.DayOfWeek;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.main.service.colon.LocatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterInstellingenDto;
import nl.rivm.screenit.model.colon.dto.KamerDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/colon/rooster")
public class ColonRoosterController
{

	private final LocatieService locatieService;

	private final ColonUitnodigingService uitnodigingService;

	private final ColonRoosterBeperkingService beperkingService;

	@GetMapping("/kamers")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<KamerDto> getKamers()
	{
		var intakeLocatie = ScreenitSession.get().getColoscopieCentrum();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("error.geen.intakelocatie");
		}

		return locatieService.getKamers(intakeLocatie).stream().map(KamerDto::fromKamer).collect(Collectors.toList());
	}

	@GetMapping("/beperkingen")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER,
		Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonRoosterBeperkingenDto> getRoosterBeperkingen()
	{
		var roosterBeperkingen = beperkingService.getRoosterBeperkingen();
		return ResponseEntity.ok(roosterBeperkingen);
	}

	@PutMapping("/beperkingen")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateRoosterBeperkingen(@RequestBody ColonRoosterBeperkingenDto roosterBeperkingenDto) throws ValidatieException
	{
		beperkingService.updateRoosterBeperkingen(roosterBeperkingenDto);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/instellingen")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ColonRoosterInstellingenDto getInstellingen()
	{
		var instellingen = new ColonRoosterInstellingenDto();
		instellingen.setGeprognosticeerdeVanafDatum(uitnodigingService.getGeprognotiseerdeIntakeDatum(true).with(DayOfWeek.MONDAY));
		instellingen.setDuurAfspraakInMinuten(ScreenitSession.get().getColoscopieCentrum().getAfspraakDefinities().get(0).getDuurAfspraakInMinuten());

		return instellingen;
	}
}
