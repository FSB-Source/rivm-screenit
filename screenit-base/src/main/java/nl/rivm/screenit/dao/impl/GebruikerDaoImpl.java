
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.dao.GebruikerDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class GebruikerDaoImpl extends AbstractAutowiredDao implements GebruikerDao
{
	@Override
	public Gebruiker getGebruikerBy(String key, String value)
	{
		Criteria crit = this.getSession().createCriteria(Gebruiker.class);
		crit.add(Restrictions.eq(key, value));
		return (Gebruiker) crit.uniqueResult();
	}

	@Override
	public Gebruiker getPatholoog(String patholoogId, Instelling instelling)
	{
		Criteria crit = this.getSession().createCriteria(Gebruiker.class);
		crit.add(Restrictions.eq("patholoogId", patholoogId));
		crit.add(Restrictions.eq("actief", true));

		List list = crit.list();
		Gebruiker gebruiker = null;
		if (list != null)
		{
			if (list.size() == 1)
			{
				gebruiker = (Gebruiker) list.get(0);
			}
			else if (list.size() > 1 && instelling != null)
			{

				boolean gebruikerNietUniek = false;
				for (Gebruiker subGebruiker : (List<Gebruiker>) list)
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
		return getSession().doReturningWork(new SequenceGenerator(DatabaseSequence.MEDEWERKERCODE, getSessionFactory())).intValue();
	}
}
