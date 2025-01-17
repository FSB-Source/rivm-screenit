
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.Collection;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.security.ScreenitRealm;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.shiro.subject.PrincipalCollection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ScopeServiceImpl implements ScopeService
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public boolean isObjectInScope(Constraint constraintToCheck, Account account, PrincipalCollection principals)
	{
		boolean result = false;
		ToegangLevel scope = getHoogsteToegangLevel(principals, constraintToCheck);
		if (scope == ToegangLevel.LANDELIJK)
		{
			result = true;
		}
		else if (Client.class.isAssignableFrom(constraintToCheck.getScopeObjectClass()))
		{

			if (account instanceof Client)
			{

				result = account.getId().equals(constraintToCheck.getScopeObjectId());
			}
			else if (scope == ToegangLevel.REGIO)
			{
				Client client = hibernateService.load(Client.class, constraintToCheck.getScopeObjectId());

				ScreeningOrganisatie screeningOrganisatie = client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
				InstellingGebruiker gebruiker = (InstellingGebruiker) account;

				result = gebruiker.getOrganisatie().equals(screeningOrganisatie);
			}
			else
			{
				result = true;
			}

		}
		else if (Instelling.class.isAssignableFrom(constraintToCheck.getScopeObjectClass()))
		{

			if (account instanceof InstellingGebruiker)
			{
				InstellingGebruiker instellingGebruiker = (InstellingGebruiker) account;
				if (scope == ToegangLevel.LANDELIJK)
				{
					result = true;
				}
				else if (scope == ToegangLevel.INSTELLING || scope == ToegangLevel.REGIO)
				{
					result = instellingGebruiker.getOrganisatie().getId().equals(constraintToCheck.getScopeObjectId());
				}
				else
				{
					result = false;
				}
			}
		}
		else if (Gemeente.class.isAssignableFrom(constraintToCheck.getScopeObjectClass()) || UitnodigingsGebied.class.isAssignableFrom(constraintToCheck.getScopeObjectClass()))
		{
			if (scope == ToegangLevel.LANDELIJK)
			{
				result = true;
			}
			else if (scope == ToegangLevel.REGIO && account instanceof InstellingGebruiker && ((InstellingGebruiker) account).getOrganisatie() instanceof ScreeningOrganisatie)
			{
				Gemeente gemeente = null;
				if (Gemeente.class.isAssignableFrom(constraintToCheck.getScopeObjectClass()))
				{
					gemeente = hibernateService.load(Gemeente.class, constraintToCheck.getScopeObjectId());
				}
				else
				{
					gemeente = hibernateService.load(UitnodigingsGebied.class, constraintToCheck.getScopeObjectId()).getGemeente();
				}

				result = ((ScreeningOrganisatie) ((InstellingGebruiker) account).getOrganisatie()).getGemeentes().contains(gemeente);
			}
			else
			{
				result = false;
			}
		}
		else
		{
			result = true;
		}

		return result;
	}

	@Override
	public ToegangLevel getHoogsteToegangLevel(PrincipalCollection principalCollection, Constraint constraintToCheck)
	{
		Permissie permissie = new Permissie();
		permissie.setActie(constraintToCheck.getActie());
		permissie.setRecht(constraintToCheck.getRecht());

		ToegangLevel toegangLevel = null;

		Collection<Permissie> perms = ApplicationContextProvider.getApplicationContext().getBean(ScreenitRealm.class).getPermissies(principalCollection);
		if (perms != null && !perms.isEmpty())
		{
			for (Permissie perm : perms)
			{
				if (perm.implies(permissie) && CollectionUtils.containsAny(Arrays.asList(perm.getRecht().getBevolkingsonderzoeken()), constraintToCheck.getBevolkingsonderzoek()))
				{
					ToegangLevel found = perm.getToegangLevel();

					if (toegangLevel == null || found.getNiveau() > toegangLevel.getNiveau())
					{
						toegangLevel = found;
					}
				}
			}
		}

		return toegangLevel;
	}

	@Override
	public ToegangLevel getHoogsteToegangLevel(InstellingGebruiker instellingGebruiker, Constraint constraintToCheck, boolean checkBvo)
	{
		Permissie permissie = new Permissie();
		permissie.setActie(constraintToCheck.getActie());
		permissie.setRecht(constraintToCheck.getRecht());

		ToegangLevel toegangLevel = null;

		Collection<Permissie> perms = ApplicationContextProvider.getApplicationContext().getBean(ScreenitRealm.class).getPermissies(instellingGebruiker, checkBvo);
		if (perms != null && !perms.isEmpty())
		{
			for (Permissie perm : perms)
			{
				if (perm.implies(permissie))
				{
					ToegangLevel found = perm.getToegangLevel();

					if (toegangLevel == null || found.getNiveau() > toegangLevel.getNiveau())
					{
						toegangLevel = found;
					}
				}
			}
		}

		return toegangLevel;
	}
}
