package nl.rivm.screenit.dao.cervix.impl;

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

import java.time.LocalDate;

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixMonsterDaoImpl extends AbstractAutowiredDao implements CervixMonsterDao
{

	@Override
	public CervixMonster getMonsterByMonsterId(String monsterId)
	{
		return getMonster(monsterId, CervixMonster.class);
	}

	private <T extends CervixMonster> T getMonster(String monsterId, Class<T> clazz)
	{
		BaseCriteria<T> baseCriteria = new BaseCriteria<>(clazz, "monster");
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
		return getMonster(monsterId, CervixUitstrijkje.class);
	}

	@Override
	public CervixZas getZas(String monsterId)
	{
		return getMonster(monsterId, CervixZas.class);

	}

	@Override
	public Long getNextMonsterId()
	{
		return getSession().doReturningWork(new SequenceGenerator(DatabaseSequence.MONSTER_ID, getSessionFactory()));
	}

	@Override
	public CervixMonster getLaatsteMonsterMetMissendeUitslagVanDossier(CervixDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		Criteria criteria = getSession().createCriteria(CervixMonster.class, "monster");
		CervixRestrictions.addMissendeUitslagRestrictions(criteria, signalerenVanaf, minimaleSignaleringsDatum);
		criteria.add(Restrictions.eq("ronde.dossier", dossier));
		criteria.addOrder(Order.desc("ontvangstdatum"));
		criteria.setMaxResults(1);
		return (CervixMonster) criteria.uniqueResult();
	}

	@Override
	public boolean isVerwijderdMonster(String monsterId)
	{
		AuditReader reader = AuditReaderFactory.get(getSession());
		AuditQuery auditQuery = reader.createQuery().forRevisionsOfEntity(CervixMonster.class, false, true)
			.add(AuditEntity.property("monsterId").eq(monsterId))
			.addProjection(AuditEntity.id().count());

		return ((Long) auditQuery.getSingleResult()) > 0;
	}
}
