package nl.rivm.screenit.main.service;

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

import nl.rivm.screenit.dto.InstellingGebruikerRolDto;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.data.domain.Sort;

public interface MedewerkerService
{
	List<InstellingGebruiker> zoekOrganisatieMedewerker(InstellingGebruiker zoekInstellingGebruiker, long first, long count, Sort sort);

	List<InstellingGebruiker> getActieveRadiologen(InstellingGebruiker zoekOrganisatieMedewerker, List<Long> exclIds, Sort sort);

	long countInstellingGebruiker(InstellingGebruiker instellingGebruiker);

	void addOrganisatieMedewerker(Instelling organisatie, Gebruiker medewerker);

	void saveOrUpdateRollen(InstellingGebruiker ingelogdeGebruiker, List<InstellingGebruikerRolDto> initieleRollen, InstellingGebruiker instellingGebruiker);

	boolean saveOrUpdateGebruiker(Gebruiker medewerker, boolean isBestaande, boolean wordGeblokkeerd);

	void inActiveerGebruiker(Gebruiker medewerker);

	boolean resetWachtwoord(Gebruiker medewerker);

	InstellingGebruiker getOrganisatieMedewerker(Instelling organisatie, Gebruiker medewerker);

	boolean zijnErInstellingGebruikersMetRol(Rol rol);

	List<Gebruiker> getActieveGebruikersMetRecht(Recht recht);

	List<InstellingGebruikerRol> getInstellingGebruikersMetRolEnBvos(Rol rol, List<Bevolkingsonderzoek> onderzoeken);

}
