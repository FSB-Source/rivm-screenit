package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import nl.rivm.screenit.main.dao.RolDao;
import nl.rivm.screenit.main.service.RolService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class RolServiceImpl implements RolService
{

	@Autowired
	private RolDao rolDao;

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void saveOrUpdateRol(Rol rol)
	{
		rolDao.saveOrUpdateRol(rol);
	}

	@Override
	public List<Rol> getToeTeVoegenRollen(InstellingGebruiker organisatieMedewerkerRolToevoegen, InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		List<Rol> toeTeVoegenRollen = new ArrayList<Rol>();

		List<Rol> rollen = getActieveRollen();
		for (Rol rol : rollen)
		{
			boolean hasRol = false;
			for (InstellingGebruikerRol organisatieMedewerkerRol : organisatieMedewerkerRolToevoegen.getRollen())
			{
				if (organisatieMedewerkerRol.isRolActief() && rol.equals(organisatieMedewerkerRol.getRol()) && organisatieMedewerkerRol.getId() != null)
				{
					hasRol = true;
					break;
				}
			}
			if (!hasRol && authorizedRol(rol, ingelogdeOrganisatieMedewerker))
			{
				toeTeVoegenRollen.add(rol);
			}
		}

		return toeTeVoegenRollen;
	}

	private boolean authorizedRol(Rol rol, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		Set<Rol> actieveRollenInstellingGebruiker = ingelogdeInstellingGebruiker.getRollen().stream().filter(InstellingGebruikerRol::getActief).map(InstellingGebruikerRol::getRol)
			.collect(Collectors.toSet());

		while (rol != null)
		{
			if (rol.getParentRol() == null && actieveRollenInstellingGebruiker.contains(rol)
				|| actieveRollenInstellingGebruiker.contains(rol.getParentRol()))
			{
				return true;
			}

			rol = rol.getParentRol(); 
		}

		return false;
	}

	@Override
	public List<Rol> getParentRollen(Rol rol)
	{
		Set<Rol> parentRollen = new HashSet<Rol>();

		for (Rol beschikbareRol : getRollen())
		{
			if (!beschikbareRol.getId().equals(rol.getId()) && Boolean.TRUE.equals(beschikbareRol.getActief()))
			{
				parentRollen.add(beschikbareRol);
			}
		}

		return new ArrayList<>(parentRollen);
	}

	@Override
	public List<Rol> getRollen()
	{
		return hibernateService.loadAll(Rol.class);
	}

	@Override
	public List<Rol> getActieveRollen()
	{
		return rolDao.getActieveRollen();
	}

	@Override
	public List<Rol> getActieveRollen(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		return rolDao.getActieveRollen(bevolkingsonderzoeken);
	}

	@Override
	public boolean opslaan(Rol rol, List<InstellingGebruikerRol> rollen, List<Bevolkingsonderzoek> verwijderdeBevolkingsonderzoek, InstellingGebruiker instellingGebruiker)
	{
		boolean opslaanGelukt = true;
		if (rollen != null && !verwijderdeBevolkingsonderzoek.isEmpty())
		{
			for (InstellingGebruikerRol igRol : rollen)
			{
				for (Bevolkingsonderzoek onderzoek : verwijderdeBevolkingsonderzoek)
				{
					igRol.getBevolkingsonderzoeken().remove(onderzoek);
				}
				if (igRol.getBevolkingsonderzoeken().isEmpty())
				{
					igRol.getBevolkingsonderzoeken().addAll(verwijderdeBevolkingsonderzoek);
					igRol.setActief(Boolean.FALSE);
				}
			}
		}
		verwijderPermissieMetBvo(rol);
		opslaanGelukt = zijnErActivePermissies(rol);
		if (opslaanGelukt)
		{
			saveLoginformatieVoorRol(rol, instellingGebruiker);
			hibernateService.saveOrUpdate(rol);
			hibernateService.saveOrUpdateAll(rollen);
		}
		return opslaanGelukt;
	}

	@Override
	public boolean zijnErRechtenDieVerwijderdWorden(Rol rol)
	{
		boolean gaatErRechtVerwijderdWorden = false;
		for (Permissie permissie : rol.getPermissies())
		{
			permissie.getRecht().getBevolkingsonderzoeken();
			if (Collections.disjoint(Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken()), rol.getBevolkingsonderzoeken()))
			{
				gaatErRechtVerwijderdWorden = true;
			}
		}
		return gaatErRechtVerwijderdWorden;
	}

	private void saveLoginformatieVoorRol(Rol rol, InstellingGebruiker instellingGebruiker)
	{
		if (rol.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.ROL_NIEUW, instellingGebruiker, "Rol: " + rol.getNaam());
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.ROL_WIJZIG, instellingGebruiker, "Rol: " + rol.getNaam());
		}
	}

	private List<Permissie> verwijderPermissieMetBvo(Rol rol)
	{
		List<Bevolkingsonderzoek> goedeOnderzoeken = rol.getBevolkingsonderzoeken();
		for (Permissie permissie : rol.getPermissies())
		{
			if (permissie.getRecht() == null || Collections.disjoint(goedeOnderzoeken, Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken())))
			{
				permissie.setActief(Boolean.FALSE);
			}
		}
		return rol.getPermissies();
	}

	private boolean zijnErActivePermissies(Rol rol)
	{
		boolean actief = false;
		for (Permissie permissie : rol.getPermissies())
		{
			if (permissie.getActief())
			{
				actief = true;
				break;
			}
		}
		return actief;
	}
}
