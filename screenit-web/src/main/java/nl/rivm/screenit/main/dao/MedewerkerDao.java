package nl.rivm.screenit.main.dao;

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

import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

public interface MedewerkerDao
{
	List<Gebruiker> searchMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<Bevolkingsonderzoek> bevolkingsonderzoeken, int first, int count, String sortProperty, boolean ascending);

	long countMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, Map<OrganisatieType, List<Instelling>> hierarchieCriteria,
		List<Bevolkingsonderzoek> bevolkingsonderzoeken);

	List<InstellingGebruiker> searchInstellingGebruiker(InstellingGebruiker instellingGebruiker, long first, long count, String orderByProperty, boolean ascending);

	List<InstellingGebruiker> getActieveRadiologen(InstellingGebruiker zoekInstellingGebruiker, List<Long> exclIds, String orderByProperty, boolean ascending);

	long countInstellingGebruiker(InstellingGebruiker instellingGebruiker);

	InstellingGebruiker getInstellingGebruiker(Instelling organisatie, Gebruiker medewerker);

	void saveOrUpdateInstellingGebruiker(InstellingGebruiker organisatieMedewerker);

	List<InstellingGebruikerRol> getInstellingGebruikersRollenMetRol(Rol rol, Date nu);

	List<Gebruiker> getActieveGebruikersMetRecht(Recht recht);

}
