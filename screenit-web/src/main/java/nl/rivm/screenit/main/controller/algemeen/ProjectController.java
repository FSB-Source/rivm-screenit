package nl.rivm.screenit.main.controller.algemeen;

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

import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.ProjectMapper;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.algemeen.dto.ProjectDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectType;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/algemeen/project")
public class ProjectController
{
	private final ProjectService projectService;

	private final ProjectMapper projectMapper;

	@GetMapping()
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.GEBRUIKER_PROJECT_OVERZICHT,
		Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }, organisatieTypeScopes = { OrganisatieType.RIVM })
	public ResponseEntity<List<ProjectDto>> getProjecten(@RequestParam(required = false) ProjectType type)
	{
		List<Project> projecten;
		if (type != null)
		{
			projecten = projectService.getProjectenVanType(type);
		}
		else
		{
			projecten = projectService.getProjecten();
		}
		var projectDtos = projecten.stream().map(projectMapper::projectToDto).collect(Collectors.toList());
		return ResponseEntity.ok().body(projectDtos);
	}

}
