package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AutorisatieServiceImpl implements AutorisatieService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ScopeService scopeService;

	@Autowired
	@Qualifier(value = "testModus")
	private Boolean testModus;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean mustChangePassword(InstellingGebruiker instellingGebruiker)
	{
		boolean mustChange = false;
		Gebruiker gebruiker = instellingGebruiker.getMedewerker();
		if (!InlogMethode.UZIPAS.equals(gebruiker.getInlogMethode()))
		{
			if (gebruiker.getLaatsteKeerWachtwoordGewijzigd() == null)
			{

				gebruiker.setLaatsteKeerWachtwoordGewijzigd(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(gebruiker);
			}
			else
			{
				Integer aantalDagen = preferenceService.getInteger(PreferenceKey.DAGEN_WACHTWOORD_GELDIG.name());
				if (aantalDagen != null && aantalDagen > 0 && getVerschilInDagen(gebruiker.getLaatsteKeerWachtwoordGewijzigd()) >= aantalDagen)
				{

					mustChange = true;
				}
			}
		}
		return mustChange;
	}

	private int getVerschilInDagen(Date laatsteKeerWachtwoordGewijzigd)
	{
		long verschil = 0;

		verschil = currentDateSupplier.getDate().getTime() - laatsteKeerWachtwoordGewijzigd.getTime();

		verschil = verschil / 1000 / 60 / 60 / 24;

		return Long.valueOf(verschil).intValue();
	}

	@Override
	public Actie getActieVoorMedewerker(InstellingGebruiker loggedInInstellingGebruiker, Gebruiker currentSelectedMedewerker, Recht... rechten)
	{

		List<Permissie> permissies = getPermissies(loggedInInstellingGebruiker, null, rechten);

		List<Actie> acties = bepaalActies(loggedInInstellingGebruiker, currentSelectedMedewerker, permissies);

		return getHoogsteActie(acties);
	}

	@Override
	public Actie getActieVoorOrganisatie(InstellingGebruiker loggedInInstellingGebruiker, Instelling currentSelectedOrganisatie, Recht... rechten)
	{

		List<Permissie> permissies = getPermissies(loggedInInstellingGebruiker, null, rechten);

		List<Actie> acties = bepaalActies(loggedInInstellingGebruiker, currentSelectedOrganisatie, permissies);

		return getHoogsteActie(acties);
	}

	private List<Permissie> getPermissies(InstellingGebruiker loggedInInstellingGebruiker, Actie minimumActie, Recht... rechten)
	{
		List<Permissie> permissies = new ArrayList<Permissie>();

		List<InstellingGebruikerRol> rollen = loggedInInstellingGebruiker.getRollen();
		if (rollen != null)
		{
			for (InstellingGebruikerRol organisatieMedewerkerRol : rollen)
			{
				if (organisatieMedewerkerRol.isRolActief())
				{
					for (Permissie permissie : organisatieMedewerkerRol.getRol().getPermissies())
					{
						if (magPermissieToevoegen(permissie, minimumActie, rechten))
						{
							permissies.add(permissie);
						}

					}
				}
			}
		}
		return permissies;
	}

	private boolean magPermissieToevoegen(Permissie permissie, Actie minimumActie, Recht... rechten)
	{
		boolean add = false;
		boolean permissieValid = false;
		if (!Boolean.FALSE.equals(permissie.getActief()) && (minimumActie == null || minimumActie.getNiveau() <= permissie.getActie().getNiveau()))
		{
			permissieValid = true;
		}

		for (Recht recht : rechten)
		{
			if ((Boolean.TRUE.equals(testModus) || !Recht.TESTEN.equals(recht)) && permissie.getRecht().equals(recht))
			{
				add = true;
			}
		}
		return add && permissieValid;
	}

	private List<Actie> bepaalActies(InstellingGebruiker loggedInInstellingGebruiker, Gebruiker currentSelectedMedewerker, List<Permissie> permissies)
	{
		List<Actie> acties = new ArrayList<Actie>();

		for (Permissie permissie : permissies)
		{
			fillRechtTypes(loggedInInstellingGebruiker, currentSelectedMedewerker, acties, permissie);
		}
		return acties;
	}

	private List<Actie> bepaalActies(InstellingGebruiker loggedInInstellingGebruiker, Instelling currentSelectedOrganisatie, List<Permissie> permissies)
	{
		List<Actie> acties = new ArrayList<Actie>();

		for (Permissie permissie : permissies)
		{
			fillRechtTypes(loggedInInstellingGebruiker, currentSelectedOrganisatie, acties, permissie);
		}
		return acties;
	}

	private void fillRechtTypes(InstellingGebruiker loggedInInstellingGebruiker, Gebruiker currentSelectedMedewerker, List<Actie> acties, Permissie permissie)
	{
		if (currentSelectedMedewerker == null)
		{
			acties.add(permissie.getActie());
		}
		else if (currentSelectedMedewerker.getId() == null)
		{
			acties.add(permissie.getActie());
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.LANDELIJK))
		{

			acties.add(permissie.getActie());
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.INSTELLING) || permissie.getToegangLevel().equals(ToegangLevel.REGIO))
		{
			bepaalActiesMedewerker(loggedInInstellingGebruiker, currentSelectedMedewerker, acties, permissie);
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.EIGEN) && currentSelectedMedewerker.getId().equals(loggedInInstellingGebruiker.getMedewerker().getId()))
		{
			acties.add(permissie.getActie());
		}
	}

	private void fillRechtTypes(InstellingGebruiker loggedInInstellingGebruiker, Instelling currentSelectedOrganisatie, List<Actie> acties, Permissie permissie)
	{
		if (currentSelectedOrganisatie == null)
		{
			acties.add(permissie.getActie());
		}
		else if (currentSelectedOrganisatie.getId() == null)
		{
			acties.add(permissie.getActie());
		}
		else
		{
			ToegangLevel toegangLevel = permissie.getToegangLevel();
			switch (toegangLevel)
			{
			case REGIO:
				switch (currentSelectedOrganisatie.getOrganisatieType())
				{
				case SCREENINGSORGANISATIE:
					if (currentSelectedOrganisatie.getId().equals(loggedInInstellingGebruiker.getOrganisatie().getId()))
					{
						acties.add(permissie.getActie());
					}
					break;
				case PA_LABORATORIUM:
					for (ColoscopieLocatie locatie : ((PaLaboratorium) currentSelectedOrganisatie).getColoscopielocaties())
					{
						if (valtBinnenRegio(loggedInInstellingGebruiker, locatie))
						{
							acties.add(permissie.getActie());
							break;
						}
					}
					break;
				case CENTRALE_EENHEID:
					if (currentSelectedOrganisatie.getRegio() == null || 
						currentSelectedOrganisatie.getRegio().equals(loggedInInstellingGebruiker.getOrganisatie()))
					{
						acties.add(permissie.getActie());
					}
					break;
				case BEOORDELINGSEENHEID:
					if (currentSelectedOrganisatie.getParent() == null || 
						currentSelectedOrganisatie.getParent().getRegio() == null || 
						currentSelectedOrganisatie.getParent().getRegio().equals(loggedInInstellingGebruiker.getOrganisatie()))
					{
						acties.add(permissie.getActie());
					}
					break;
				default:
					if (valtBinnenRegio(loggedInInstellingGebruiker, currentSelectedOrganisatie))
					{
						acties.add(permissie.getActie());
					}
					break;
				}
				break;
			case INSTELLING:
				if (currentSelectedOrganisatie.getId().equals(loggedInInstellingGebruiker.getOrganisatie().getId()))
				{
					acties.add(permissie.getActie());
				}
				break;
			default:

				acties.add(permissie.getActie());
				break;
			}
		}
	}

	private boolean valtBinnenRegio(InstellingGebruiker loggedInInstellingGebruiker, Instelling currentSelectedOrganisatie)
	{
		boolean valtBinnenRegio = false;

		Instelling parentSelected = currentSelectedOrganisatie;
		Instelling parentIngelogdeOrganisatie = loggedInInstellingGebruiker.getOrganisatie();

		if (parentSelected != null && parentSelected.getId().equals(parentIngelogdeOrganisatie.getId()))
		{
			valtBinnenRegio = true;
		}
		if (!valtBinnenRegio)
		{
			while (parentSelected != null && parentSelected.getOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
			{
				parentSelected = parentSelected.getParent();
			}
			while (parentIngelogdeOrganisatie != null && parentIngelogdeOrganisatie.getOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
			{
				parentIngelogdeOrganisatie = parentIngelogdeOrganisatie.getParent();
			}

			if (parentSelected != null && parentIngelogdeOrganisatie != null && parentSelected.getId().equals(parentIngelogdeOrganisatie.getId()))
			{
				valtBinnenRegio = true;
			}
		}
		return valtBinnenRegio;
	}

	private void bepaalActiesMedewerker(InstellingGebruiker loggedInInstellingGebruiker, Gebruiker currentSelectedMedewerker, List<Actie> acties, Permissie permissie)
	{

		if (CollectionUtils.isNotEmpty(currentSelectedMedewerker.getOrganisatieMedewerkers()))
		{
			for (InstellingGebruiker instellingMedewerker : currentSelectedMedewerker.getOrganisatieMedewerkers())
			{
				Instelling organisatie = instellingMedewerker.getOrganisatie();
				if (organisatie.getId().equals(instellingMedewerker.getOrganisatie().getId()))
				{
					acties.add(permissie.getActie());
				}
			}
		}
	}

	private Actie getHoogsteActie(List<Actie> acties)
	{
		Actie result = null;
		for (Actie actie : acties)
		{
			if (result == null)
			{
				result = actie;
			}
			else if (actie.getNiveau() > result.getNiveau())
			{
				result = actie;
			}
		}
		return result;
	}

	@Override
	public List<OrganisatieType> getOrganisatieTypes(InstellingGebruiker instellingGebruiker, boolean checkBvo)
	{
		return getOrganisatieTypes(instellingGebruiker, Actie.INZIEN, checkBvo);
	}

	@Override
	public List<OrganisatieType> getOrganisatieTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo)
	{
		List<OrganisatieType> organisatieTypes = new ArrayList<>();

		for (OrganisatieType organisatieType : OrganisatieType.values())
		{

			ToegangLevel level = getToegangLevel(instellingGebruiker, minimumActie, checkBvo, organisatieType.getRecht());
			if (level != null)
			{
				organisatieTypes.add(organisatieType);
			}
		}
		return organisatieTypes;
	}

	@Override
	public ToegangLevel getToegangLevel(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo, Recht... rechten)
	{
		Constraint constraint = new Constraint();
		constraint.setActie(minimumActie);
		constraint.setBevolkingsonderzoek(instellingGebruiker.getBevolkingsonderzoeken());
		ToegangLevel hoogsteOveralToegangLevel = null;
		for (Recht recht : rechten)
		{
			constraint.setRecht(recht);

			ToegangLevel hoogsteToegangLevel = scopeService.getHoogsteToegangLevel(instellingGebruiker, constraint, checkBvo);
			if (hoogsteOveralToegangLevel == null)
			{
				hoogsteOveralToegangLevel = hoogsteToegangLevel;
			}
			if (hoogsteToegangLevel != null)
			{
				if (hoogsteOveralToegangLevel != null && hoogsteToegangLevel.getNiveau() > hoogsteOveralToegangLevel.getNiveau())
				{
					hoogsteOveralToegangLevel = hoogsteToegangLevel;
				}
				if (hoogsteToegangLevel.equals(ToegangLevel.LANDELIJK))
				{

					break;
				}
			}
		}
		return hoogsteOveralToegangLevel;
	}

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken(InstellingGebruiker instellingGebruiker)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		for (InstellingGebruikerRol rol : instellingGebruiker.getRollen())
		{
			if (!rol.isRolActief())
			{
				continue;
			}
			for (Bevolkingsonderzoek onderzoek : rol.getBevolkingsonderzoeken())
			{
				if (!onderzoeken.contains(onderzoek))
				{
					onderzoeken.add(onderzoek);
				}
			}
		}
		return onderzoeken;
	}

	@Override
	public List<Recht> getRechtWithBevolkingsonderzoek(List<Bevolkingsonderzoek> onderzoeken)
	{
		List<Recht> rechten = new ArrayList<Recht>();
		for (Recht recht : Recht.values())
		{
			for (Bevolkingsonderzoek bevolkingsonderzoek : recht.getBevolkingsonderzoeken())
			{
				if (onderzoeken != null && onderzoeken.contains(bevolkingsonderzoek))
				{
					rechten.add(recht);
					break;
				}
				else if (onderzoeken == null)
				{
					rechten.add(recht);
				}
			}
		}
		return rechten;
	}
}
