package nl.rivm.screenit.main.service.algemeen.impl;

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

import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.dao.MedewerkerDao;
import nl.rivm.screenit.main.service.algemeen.MedewerkerZoekService;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.OrganisatieZoekService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class MedewerkerZoekServiceImpl implements MedewerkerZoekService
{
	private final MedewerkerDao medewerkerDao;

	private final OrganisatieZoekService organisatieZoekService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Override
	public List<Gebruiker> searchMedewerkers(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, InstellingGebruiker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen, int first, int count, String sortProperty, boolean ascending)
	{
		return medewerkerDao.searchMedewerkers(zoekObject, selectedFuncties, selectedRollen,
			getHierarchieCriteria(ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen),
			getBevolkingsonderzoekenZoekParameter(ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen),
			first, count, sortProperty, ascending);
	}

	@Override
	public long countMedewerkers(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, InstellingGebruiker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen)
	{
		return medewerkerDao.countMedewerkers(zoekObject, selectedFuncties, selectedRollen,
			getHierarchieCriteria(ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen),
			getBevolkingsonderzoekenZoekParameter(ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen));
	}

	private Map<OrganisatieType, List<Instelling>> getHierarchieCriteria(InstellingGebruiker loggedInInstellingGebruiker, boolean voorOrganisatieKoppelen)
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = new EnumMap<>(OrganisatieType.class);
		if (!voorOrganisatieKoppelen)
		{
			var toegangLevel = autorisatieService.getToegangLevel(loggedInInstellingGebruiker, Actie.INZIEN, true, Recht.GEBRUIKER_MEDEWERKER_BEHEER);

			for (var type : OrganisatieType.values())
			{
				var toegangsniveau = toegangLevel.getNiveau();
				var addToHierarchieCriteria = switch (type)
				{
					case RIVM, INPAKCENTRUM, LABORATORIUM -> toegangsniveau == ToegangLevel.LANDELIJK.getNiveau();
					case SCREENINGSORGANISATIE -> toegangsniveau >= ToegangLevel.REGIO.getNiveau();
					default -> toegangsniveau >= ToegangLevel.INSTELLING.getNiveau();
				};
				if (addToHierarchieCriteria)
				{
					hierarchieCriteria.put(type, organisatieZoekService.getOrganisatiesForNiveau(loggedInInstellingGebruiker, type, toegangLevel));
				}
			}
		}
		return hierarchieCriteria;
	}

	private List<Bevolkingsonderzoek> getBevolkingsonderzoekenZoekParameter(InstellingGebruiker ingelogdeOrganisatieMedewerker, boolean voorOrganisatieKoppelen)
	{
		if (voorOrganisatieKoppelen)
		{
			return Collections.emptyList();
		}
		return ingelogdeOrganisatieMedewerker.getBevolkingsonderzoeken();
	}

}
