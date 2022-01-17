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

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixMonsterDaoImpl extends AbstractAutowiredDao implements CervixMonsterDao
{

	@Override
	public CervixMonster getMonsterByMonsterId(String monsterId)
	{
		BaseCriteria<CervixMonster> baseCriteria = new BaseCriteria<>(CervixMonster.class, "monster");
		baseCriteria.add(Restrictions.eq("monster.monsterId", monsterId.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public CervixUitstrijkje getUitstrijkjeByClientBsnAndMonsterId(String bsn, String monsterId)
	{
		BaseCriteria<CervixUitstrijkje> baseCriteria = getCervixUitstrijkjeBaseCriteria();

		baseCriteria.add(Restrictions.eq("persoon.bsn", bsn));
		baseCriteria.add(Restrictions.eq("uitstrijkje.monsterId", monsterId.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public CervixUitstrijkje getUitstrijkjeByClientBsnAndControleLetters(String bsn,
		String controleLetters)
	{
		BaseCriteria<CervixUitstrijkje> baseCriteria = getCervixUitstrijkjeBaseCriteria();

		baseCriteria.add(Restrictions.eq("persoon.bsn", bsn));
		baseCriteria.add(Restrictions.eq("uitstrijkje.controleLetters", controleLetters.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	private BaseCriteria<CervixUitstrijkje> getCervixUitstrijkjeBaseCriteria()
	{
		BaseCriteria<CervixUitstrijkje> baseCriteria = new BaseCriteria<>(CervixUitstrijkje.class, "uitstrijkje");
		baseCriteria.createAlias("uitstrijkje.uitnodiging", "uitnodiging");
		baseCriteria.createAlias("uitnodiging.screeningRonde", "ronde");
		baseCriteria.createAlias("ronde.dossier", "dossier");
		baseCriteria.createAlias("dossier.client", "client");
		baseCriteria.createAlias("client.persoon", "persoon");
		return baseCriteria;
	}

	@Override
	public CervixUitstrijkje getUitstrijkje(String monsterId)
	{
		BaseCriteria<CervixUitstrijkje> baseCriteria = new BaseCriteria<>(CervixUitstrijkje.class, "monster");
		baseCriteria.add(Restrictions.eq("monster.monsterId", monsterId.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public CervixZas getZas(String monsterId)
	{
		BaseCriteria<CervixZas> baseCriteria = new BaseCriteria<>(CervixZas.class, "monster");
		baseCriteria.add(Restrictions.eq("monster.monsterId", monsterId.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public Long getNextMonsterId()
	{
		return getSession().doReturningWork(new SequenceGenerator(DatabaseSequence.MONSTER_ID, getSessionFactory()));
	}
}
