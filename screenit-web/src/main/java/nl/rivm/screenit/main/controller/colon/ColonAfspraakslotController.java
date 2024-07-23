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
import java.time.LocalTime;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/colon/rooster/afspraakslot")
public class ColonAfspraakslotController
{
	private final ColonAfspraakslotService afspraakslotService;

	@GetMapping
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonAfspraakslotDto> getAfspraakslots(@RequestParam("startDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
		@RequestParam("endDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate)
	{
		var intakeLocatie = ScreenitSession.get().getColoscopieCentrum();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("Gebruiker heeft geen intakelocatie.");
		}

		if (startDate == null)
		{
			throw new IllegalStateException("Er is geen start datum meegegeven");
		}

		if (endDate == null)
		{
			throw new IllegalStateException("Er is geen eind datum meegegeven");
		}

		return afspraakslotService.getAfspraakslots(startDate, endDate, intakeLocatie);
	}

	@GetMapping("/search")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonTijdslotDto> searchAfspraakslots(@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDatum,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate eindDatum,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.TIME) LocalTime startTijd,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.TIME) LocalTime eindTijd,
		@RequestParam(required = false) Long kamerId, @RequestParam() String dagen)
	{
		var intakelocatie = ScreenitSession.get().getColoscopieCentrum();

		if (intakelocatie == null)
		{
			throw new IllegalStateException("Gebruiker heeft geen intakelocatie.");
		}

		var filter = new RoosterListViewFilter();
		filter.setStartDatum(DateUtil.toUtilDate(startDatum));
		filter.setEindDatum(DateUtil.toUtilDate(eindDatum));
		filter.setStartTijd(startTijd);
		filter.setEindTijd(eindTijd);
		filter.setKamerId(kamerId);
		filter.setDagen(Stream.of(dagen.split(","))
			.map(String::trim)
			.map(Integer::parseInt)
			.collect(Collectors.toList()));

		return afspraakslotService.searchAfspraakslots(filter, intakelocatie.getId());
	}

	@PostMapping
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> createAfspraakslots(@RequestBody ColonAfspraakslotDto afspraakslotsDto)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException, BulkAanmakenException
	{
		afspraakslotService.createAfspraakslot(afspraakslotsDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.CREATED).build();
	}

	@PutMapping("{id}")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateAfspraakslot(@PathVariable("id") Long id, @RequestBody ColonAfspraakslotDto afspraakslotDto)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		afspraakslotService.updateAfspraakslot(id, afspraakslotDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
	}

	@DeleteMapping("{ids}")
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteAfspraakslots(@PathVariable("ids") String ids, @RequestParam(required = false) Boolean alleenValidatie,
		@RequestParam(required = false) Boolean bulk)
		throws BulkVerwijderenException, ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		var afspraakslotIds = Stream.of(ids.split(","))
			.map(String::trim)
			.map(Long::parseLong)
			.collect(Collectors.toList());

		if (Boolean.TRUE.equals(bulk))
		{
			afspraakslotService.bulkDeleteAfspraakslots(afspraakslotIds, ScreenitSession.get().getLoggedInInstellingGebruiker(), alleenValidatie);
		}
		else
		{
			afspraakslotService.deleteAfspraakslot(afspraakslotIds.get(0), ScreenitSession.get().getLoggedInInstellingGebruiker());
		}

		return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
	}
}
