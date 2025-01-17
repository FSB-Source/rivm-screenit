package nl.rivm.screenit.main.service.impl;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.PermissieDto;
import nl.rivm.screenit.dto.RolDto;
import nl.rivm.screenit.main.service.RolService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.Rol_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.RolRepository;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.RolSpecification.filterBevolkingsonderzoek;
import static nl.rivm.screenit.specification.algemeen.RolSpecification.isActief;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@AllArgsConstructor
public class RolServiceImpl implements RolService
{
	private final RolRepository rolRepository;

	private final LogService logService;

	private final HibernateService hibernateService;

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void setRechtActiefOfInactief(Rol rol, Account ingelogdeAccount)
	{
		boolean actief = rol.getActief();
		long rolId = rol.getId();
		hibernateService.getHibernateSession().evict(rol);
		Rol opslaanRol = hibernateService.get(Rol.class, rolId);
		opslaanRol.setActief(actief);
		saveLoginformatieVoorActieverenRol(opslaanRol, ingelogdeAccount);
		hibernateService.saveOrUpdate(opslaanRol);
	}

	@Override
	public List<Rol> getToeTeVoegenRollen(InstellingGebruiker organisatieMedewerkerRolToevoegen, InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		List<Rol> toeTeVoegenRollen = new ArrayList<>();

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
		Set<Rol> parentRollen = new HashSet<>();

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
		return getActieveRollen(null);
	}

	@Override
	public List<Rol> getActieveRollen(Collection<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		return rolRepository.findWith(isActief(true).and(filterBevolkingsonderzoek(bevolkingsonderzoeken)), q -> q)
			.fetch(g -> g.addSubgraph(Rol_.permissies))
			.all();
	}

	@Override
	public boolean opslaan(Rol rol, List<InstellingGebruikerRol> rollen, RolDto initieleRol, List<Bevolkingsonderzoek> verwijderdeBevolkingsonderzoek,
		InstellingGebruiker ingelogdeInstellingGebruiker)
	{
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
		var opslaanGelukt = zijnErActivePermissies(rol);
		if (opslaanGelukt)
		{
			saveLogInformatieVoorRol(initieleRol, rol, ingelogdeInstellingGebruiker);
			hibernateService.saveOrUpdate(rol);
			hibernateService.saveOrUpdateAll(rollen);
		}
		return opslaanGelukt;
	}

	@Override
	public boolean zijnErRechtenDieVerwijderdWorden(Rol rol)
	{
		return rol.getPermissies()
			.stream()
			.anyMatch(permissie -> Collections.disjoint(Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken()), rol.getBevolkingsonderzoeken()));
	}

	private void saveLogInformatieVoorRol(RolDto initieleRol, Rol rol, InstellingGebruiker instellingGebruiker)
	{
		if (rol.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.ROL_NIEUW, instellingGebruiker, String.format("Rol: %s", rol.getNaam()));
		}
		else if (isRolGewijzigd(initieleRol, rol))
		{
			var melding = String.format("Rol: %s gewijzigd ", rol.getNaam());
			var wijzigingen = new StringJoiner(", ", "(", ")");

			toevoegenWijzigingen(initieleRol, rol, wijzigingen);
			melding += wijzigingen.toString();
			logService.logGebeurtenis(LogGebeurtenis.ROL_WIJZIG, instellingGebruiker, melding);
		}
		rol.getPermissies().forEach(permissie -> saveLogInformatieVoorPermissie(rol, initieleRol, permissie, instellingGebruiker));
	}

	private static void toevoegenWijzigingen(RolDto initieleRol, Rol rol, StringJoiner wijzigingen)
	{
		if (!Objects.equals(initieleRol.getNaam(), rol.getNaam()))
		{
			wijzigingen.add(String.format("Naam: %s -> %s", initieleRol.getNaam(), rol.getNaam()));
		}
		if (!Objects.equals(initieleRol.getParentRol() == null ? null : initieleRol.getParentRol().getId(), rol.getParentRol() == null ? null : rol.getParentRol().getId()))
		{
			wijzigingen.add(String.format("Alleen beschikbaar voor: %s -> %s", initieleRol.getParentRol() == null ? "(leeg)" : initieleRol.getParentRol().getNaam(),
				rol.getParentRol() == null ? "(leeg)" : rol.getParentRol().getNaam()));
		}
		if (!Objects.equals(Bevolkingsonderzoek.getAfkortingen(initieleRol.getBevolkingsonderzoeken()), Bevolkingsonderzoek.getAfkortingen(rol.getBevolkingsonderzoeken())))
		{
			wijzigingen.add(String.format("Bevolkingsonderzoeken: %s -> %s", Bevolkingsonderzoek.getAfkortingen(initieleRol.getBevolkingsonderzoeken()),
				Bevolkingsonderzoek.getAfkortingen(rol.getBevolkingsonderzoeken())));
		}
	}

	private void saveLogInformatieVoorPermissie(Rol rol, RolDto initieleRol, Permissie permissie, InstellingGebruiker instellingGebruiker)
	{
		if (permissie.getRecht() != null)
		{
			var initielePermissieOptional = initieleRol.getPermissies().stream().filter(p -> Objects.equals(p.getId(), permissie.getId())).findFirst();

			if ((initielePermissieOptional.isEmpty() || initielePermissieOptional.get().getId() == null) && !Boolean.TRUE.equals(permissie.getActief()))
			{
				return;
			}

			var melding = String.format("Rol: %s. Recht '%s' ", rol.getNaam(), permissie.getRecht().getNaam());

			if (initielePermissieOptional.isEmpty() || initielePermissieOptional.get().getId() == null)
			{
				melding += String.format("toegevoegd met actie '%s' en niveau '%s'", permissie.getActie().getNaam(), permissie.getToegangLevel().getNaam());
				logService.logGebeurtenis(LogGebeurtenis.ROL_WIJZIG, instellingGebruiker, melding);
				return;
			}

			var initielePermissie = initielePermissieOptional.get();
			if (Boolean.FALSE.equals(initielePermissie.getActief()) && Boolean.FALSE.equals(permissie.getActief()) || (Boolean.TRUE.equals(permissie.getActief())
				&& !isPermissieGewijzigd(initielePermissie, permissie)))
			{
				return;
			}

			melding = aanvullenMelding(permissie, melding, initielePermissie);
			logService.logGebeurtenis(LogGebeurtenis.ROL_WIJZIG, instellingGebruiker, melding);
		}
	}

	private String aanvullenMelding(Permissie permissie, String melding, PermissieDto initielePermissie)
	{
		if (Boolean.FALSE.equals(permissie.getActief()))
		{
			melding += "verwijderd";
		}
		else if (isPermissieGewijzigd(initielePermissie, permissie))
		{
			melding += "gewijzigd ";
			var wijzigingen = new StringJoiner(", ", "(", ")");

			if (permissie.getActie() != initielePermissie.getActie())
			{
				wijzigingen.add(String.format("Actie: %s -> %s", initielePermissie.getActie().getNaam(), permissie.getActie().getNaam()));
			}
			if (permissie.getToegangLevel() != initielePermissie.getToegangLevel())
			{
				wijzigingen.add(String.format("Niveau: %s -> %s", initielePermissie.getToegangLevel().getNaam(), permissie.getToegangLevel().getNaam()));
			}
			melding += wijzigingen.toString();
		}
		return melding;
	}

	private boolean isRolGewijzigd(RolDto initieleRol, Rol rol)
	{
		return !Objects.equals(initieleRol.getNaam(), rol.getNaam()) ||
			!Objects.equals(Bevolkingsonderzoek.getAfkortingen(initieleRol.getBevolkingsonderzoeken()), Bevolkingsonderzoek.getAfkortingen(rol.getBevolkingsonderzoeken())) ||
			!Objects.equals(initieleRol.getParentRol() == null ? null : initieleRol.getParentRol().getId(), rol.getParentRol() == null ? null : rol.getParentRol().getId());
	}

	private boolean isPermissieGewijzigd(PermissieDto initielePermissie, Permissie permissie)
	{
		return permissie.getActie() != initielePermissie.getActie() || permissie.getToegangLevel() != initielePermissie.getToegangLevel();
	}

	private void verwijderPermissieMetBvo(Rol rol)
	{
		List<Bevolkingsonderzoek> goedeOnderzoeken = rol.getBevolkingsonderzoeken();
		rol.getPermissies().stream()
			.filter(permissie -> permissie.getRecht() == null || Collections.disjoint(goedeOnderzoeken, Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken())))
			.forEach(permissie -> permissie.setActief(Boolean.FALSE));
	}

	private boolean zijnErActivePermissies(Rol rol)
	{
		boolean actief = false;
		for (Permissie permissie : rol.getPermissies())
		{
			if (Boolean.TRUE.equals(permissie.getActief()))
			{
				actief = true;
				break;
			}
		}
		return actief;
	}

	private void saveLoginformatieVoorActieverenRol(Rol rol, Account ingelogdeAccount)
	{
		if (Boolean.FALSE.equals(rol.getActief()))
		{
			rol.setActief(Boolean.TRUE);
			logService.logGebeurtenis(LogGebeurtenis.ROL_ACTIVEREN, ingelogdeAccount, "Rol: " + rol.getNaam());
		}
		else
		{
			rol.setActief(Boolean.FALSE);
			logService.logGebeurtenis(LogGebeurtenis.ROL_VERWIJDEREN, ScreenitSession.get().getLoggedInAccount(), "Rol: " + rol.getNaam());
		}
	}
}
