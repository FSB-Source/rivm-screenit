package nl.rivm.screenit.service.impl;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.dao.GebruikerDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.GebruikersService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class GebruikersServiceImpl implements GebruikersService
{

	@Autowired
	private GebruikerDao gebruikerDao;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public Gebruiker getGebruikerByGebruikersnaam(String gebruikersnaam)
	{
		return getGebruikerBy("gebruikersnaam", gebruikersnaam);
	}

	@Override
	public Gebruiker getGebruikerBy(String key, String value)
	{
		return gebruikerDao.getGebruikerBy(key, value);
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(Gebruiker gebruiker)
	{
		List<ScreeningOrganisatie> screeningOrganisaties = new ArrayList<>();
		for (InstellingGebruiker instellingGebruiker : gebruiker.getOrganisatieMedewerkers())
		{
			if (ScreeningOrganisatie.class.isAssignableFrom(Hibernate.getClass(instellingGebruiker.getOrganisatie())))
			{
				screeningOrganisaties.add((ScreeningOrganisatie) instellingGebruiker.getOrganisatie());
			}
		}

		if (screeningOrganisaties.size() == 1)
		{
			return screeningOrganisaties.get(0);
		}

		return null;
	}

	@Override
	public Gebruiker getGebruikerByUzinummer(String uzinummer)
	{
		return getGebruikerBy("uzinummer", uzinummer);
	}

	@Override
	public Gebruiker getPatholoog(String patholoogId, Instelling instelling)
	{
		return gebruikerDao.getPatholoog(patholoogId, instelling);
	}

	@Override
	public int getNextMedewerkercode()
	{
		return gebruikerDao.getNextMedewerkercode();
	}
}
