package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;

public interface AutorisatieService
{

	boolean mustChangePassword(InstellingGebruiker instellingGebruiker);

	Actie getActieVoorMedewerker(InstellingGebruiker loggedInInstellingGebruiker, Gebruiker currentSelectedMedewerker, Recht... rechten);

	Actie getActieVoorOrganisatie(InstellingGebruiker loggedInInstellingGebruiker, Instelling currentSelectedOrganisatie, Recht... rechten);

	ToegangLevel getToegangLevel(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo, Recht... rechten);

	List<OrganisatieType> getOrganisatieTypes(InstellingGebruiker instellingGebruiker, boolean checkBvo);

	List<OrganisatieType> getOrganisatieTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo);

	List<Bevolkingsonderzoek> getBevolkingsonderzoeken(InstellingGebruiker instellingGebruiker);

	List<Recht> getRechtWithBevolkingsonderzoek(List<Bevolkingsonderzoek> onderzoeken);

}
