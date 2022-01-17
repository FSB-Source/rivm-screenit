package nl.rivm.screenit.dao.cervix.impl;

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

import nl.rivm.screenit.dao.cervix.CervixHuisartsSyncDao;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsSyncDaoImpl extends AbstractAutowiredDao implements CervixHuisartsSyncDao
{

	@Override
	public InstellingGebruiker findInstellingGebruikerByName(String naam, Long id)
	{

		Criteria crit = getSession().createCriteria(InstellingGebruiker.class);
		crit.createAlias("medewerker", "medewerker");
		crit.createAlias("organisatie", "organisatie");
		crit.add(Restrictions.eq("organisatie.id", id));
		crit.add(Restrictions.eq("medewerker.naam", naam));
		return (InstellingGebruiker) crit.uniqueResult();
	}

	@Override
	public CervixHuisartsLocatie locatieFindByHuisartsportaalId(Long id)
	{
		return (CervixHuisartsLocatie) getSession().createCriteria(CervixHuisartsLocatie.class).add(Restrictions.eq("huisartsportaalId", id)).uniqueResult();
	}

	@Override
	public CervixHuisartsAdres adresFindByHuisartsportaalId(Long id)
	{
		return (CervixHuisartsAdres) getSession().createCriteria(CervixHuisartsAdres.class).add(Restrictions.eq("huisartsportaalId", id)).uniqueResult();
	}

	@Override
	public CervixHuisarts huisartsFindByHuisartsportaalId(Long id)
	{
		return (CervixHuisarts) getSession().createCriteria(CervixHuisarts.class).add(Restrictions.eq("huisartsportaalId", id)).uniqueResult();
	}

	@Override
	public CervixLabformulierAanvraag aanvraagFindByHuisartsportaalId(Long id)
	{
		return (CervixLabformulierAanvraag) getSession().createCriteria(CervixLabformulierAanvraag.class).add(Restrictions.eq("huisartsportaalId", id)).uniqueResult();
	}
}
