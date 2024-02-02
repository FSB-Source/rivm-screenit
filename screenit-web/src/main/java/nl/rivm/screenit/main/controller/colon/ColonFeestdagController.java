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

import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonFeestdagService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonFeestdag;
import nl.rivm.screenit.model.colon.dto.ColonFeestdagDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

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
@RestController
@AllArgsConstructor
@RequestMapping("/api/colon/feestdag")
public class ColonFeestdagController
{
	private final ColonFeestdagService feestdagService;

	@GetMapping
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.COLON_FEESTDAGEN_BEHEER,
		Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<List<ColonFeestdag>> getFeestdagen(@RequestParam Optional<String> startDatum, @RequestParam Optional<String> eindDatum)
	{
		var feestdagen = startDatum.isPresent() && eindDatum.isPresent() ? feestdagService.getFeestdagen(startDatum.get(), eindDatum.get()) : feestdagService.getFeestdagen();
		return ResponseEntity.ok(feestdagen);
	}

	@PostMapping
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonFeestdag> createFeestdag(@RequestBody ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var nieuweFeestdag = feestdagService.createFeestdag(feestdagDto);
		return ResponseEntity.ok(nieuweFeestdag);
	}

	@PutMapping("{id}")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonFeestdag> updateFeestdag(@PathVariable long id, @RequestBody ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var updatedFeestdag = feestdagService.updateFeestdag(id, feestdagDto);
		return ResponseEntity.ok(updatedFeestdag);
	}

	@DeleteMapping("{id}")
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteFeestdag(@PathVariable long id) throws ValidatieException
	{
		feestdagService.deleteFeestdag(id);
		return ResponseEntity.ok().build();
	}
}
