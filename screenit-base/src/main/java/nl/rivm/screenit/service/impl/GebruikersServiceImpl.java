package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class GebruikersServiceImpl implements GebruikersService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private GebruikerRepository gebruikerRepository;

	@Override
	public Optional<Gebruiker> getGebruikerByGebruikersnaam(String gebruikersnaam)
	{
		return gebruikerRepository.findByGebruikersnaam(gebruikersnaam);
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
	public Optional<Gebruiker> getGebruikerByUzinummer(String uzinummer)
	{
		return gebruikerRepository.findByUzinummer(uzinummer);
	}

	@Override
	public Gebruiker getPatholoog(String patholoogId, Instelling instelling)
	{
		var pathologen = gebruikerRepository.findByPatholoogIdAndActiefTrue(patholoogId);
		Gebruiker gebruiker = null;
		if (pathologen != null)
		{
			if (pathologen.size() == 1)
			{
				gebruiker = pathologen.get(0);
			}
			else if (pathologen.size() > 1 && instelling != null)
			{

				boolean gebruikerNietUniek = false;
				for (Gebruiker subGebruiker : pathologen)
				{
					if (subGebruiker.getOrganisatieMedewerkers() != null)
					{
						for (InstellingGebruiker instellingGebruiker : subGebruiker.getOrganisatieMedewerkers())
						{
							if (Boolean.TRUE.equals(instellingGebruiker.getActief()) && Boolean.TRUE.equals(instellingGebruiker.getOrganisatie().getActief())
								&& instellingGebruiker.getOrganisatie().equals(instelling))
							{
								if (gebruiker == null)
								{
									gebruiker = subGebruiker;
								}
								else
								{
									gebruikerNietUniek = true;
								}
							}
						}
					}
				}
				if (gebruikerNietUniek)
				{
					gebruiker = null;
				}
			}
		}
		return gebruiker;
	}

	@Override
	public int getNextMedewerkercode()
	{
		return hibernateService.getHibernateSession()
			.doReturningWork(new SequenceGenerator(DatabaseSequence.MEDEWERKERCODE, hibernateService.getHibernateSession().getSessionFactory())).intValue();
	}
}
