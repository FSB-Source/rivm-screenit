package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixBepaalVervolgDaoImpl extends AbstractAutowiredDao implements CervixBepaalVervolgDao
{
	private <T extends CervixMonster> BaseCriteria<T> ontvangenMonsters(Class<T> clazz, CervixScreeningRonde ontvangstRonde, boolean ongeldig, boolean pap0, T andersDan)
	{

		BaseCriteria<T> criteria = new BaseCriteria<>(clazz, "monster");
		criteria.alias("monster.ontvangstScreeningRonde", "ontvangstRonde");
		criteria.add(Restrictions.eq("ontvangstRonde.id", ontvangstRonde.getId()));

		Disjunction disjunction = Restrictions.disjunction();
		criteria.add(disjunction);
		if (ongeldig)
		{
			criteria.alias("monster.laatsteHpvBeoordeling", "hpvBeoordeling", JoinType.LEFT_OUTER_JOIN);
			disjunction.add(Restrictions.eq("hpvBeoordeling.hpvUitslag", CervixHpvUitslag.ONGELDIG));
		}
		if (pap0)
		{
			criteria.alias("monster.cytologieVerslag", "cytologieVerslag", JoinType.LEFT_OUTER_JOIN);
			disjunction.add(Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP0));

			criteria.add(Restrictions.or(
				Restrictions.isNull("ontvangstRonde.inVervolgonderzoekDatum"),
				Restrictions.ltProperty("ontvangstRonde.inVervolgonderzoekDatum", "monster.ontvangstdatum")));
		}

		if (andersDan != null)
		{
			criteria.add(Restrictions.ne("monster.id", andersDan.getId()));
		}

		return criteria;
	}

	public boolean andereZasOngeldig(CervixZas zas)
	{
		BaseCriteria<CervixZas> criteria = ontvangenMonsters(CervixZas.class, zas.getOntvangstScreeningRonde(), true, false, zas);
		return criteria.count(getSession()) > 0;
	}

	public boolean anderUitstrijkjeOnbeoordeelbaar(CervixUitstrijkje uitstrijkje)
	{
		BaseCriteria<CervixUitstrijkje> criteria = ontvangenMonsters(CervixUitstrijkje.class, uitstrijkje.getOntvangstScreeningRonde(), true, true, uitstrijkje);
		return criteria.count(getSession()) > 0;
	}

	public boolean anderUitstrijkjeOnbeoordeelbaarCytologie(CervixUitstrijkje uitstrijkje)
	{
		BaseCriteria<CervixUitstrijkje> criteria = ontvangenMonsters(CervixUitstrijkje.class, uitstrijkje.getOntvangstScreeningRonde(), false, true, uitstrijkje);
		return criteria.count(getSession()) > 0;
	}

	public boolean uitstrijkjeOnbeoordeelbaarCytologie(CervixScreeningRonde ontvangstRonde)
	{
		BaseCriteria<CervixUitstrijkje> criteria = ontvangenMonsters(CervixUitstrijkje.class, ontvangstRonde, false, true, null);
		return criteria.count(getSession()) > 0;
	}
}
