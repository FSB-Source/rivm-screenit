package nl.rivm.screenit.main.controller.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.MammaDense2ConfiguratieDto;
import nl.rivm.screenit.main.service.mamma.MammaDense2Service;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/mamma/dense2")
public class MammaDense2Controller
{
	private final MammaBaseDense2Service baseDense2Service;

	private final MammaDense2Service dense2Service;

	@GetMapping("/configuratie")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public MammaDense2ConfiguratieDto getConfiguratie()
	{
		return baseDense2Service.getConfiguratie();
	}

	@PutMapping("/configuratie")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> updateConfiguratie(@RequestBody MammaDense2ConfiguratieDto configuratie)
	{
		var instellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		baseDense2Service.updateConfiguratie(configuratie, instellingGebruiker);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/export")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Resource> getExport()
	{
		var file = baseDense2Service.getExport();
		var resource = new FileSystemResource(file);
		var headers = new HttpHeaders();
		headers.add(HttpHeaders.CONTENT_TYPE, "text/csv");
		headers.add(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + file.getName());
		return ResponseEntity.ok().headers(headers).body(resource);
	}

	@PostMapping("/import")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<String> importClienten(@RequestParam(value = "file") MultipartFile file)
	{
		try
		{
			var melding = dense2Service.importClienten(file, ScreenitSession.get().getLoggedInInstellingGebruiker());
			return ResponseEntity.ok().body(melding);
		}
		catch (Exception ex)
		{
			LOG.error("Fout bij importeren DENSE2 populatie", ex);
			return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).build();
		}
	}
}
