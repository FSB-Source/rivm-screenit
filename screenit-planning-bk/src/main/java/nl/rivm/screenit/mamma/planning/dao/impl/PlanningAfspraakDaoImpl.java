package nl.rivm.screenit.mamma.planning.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.Collection;
import java.util.List;

import nl.rivm.screenit.mamma.planning.dao.PlanningAfspraakDao;
import nl.rivm.screenit.mamma.planning.dao.dto.AfspraakDaoDto;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningAfspraakDaoImpl extends AbstractAutowiredDao implements PlanningAfspraakDao
{
	@Override
	public List<AfspraakDaoDto> afsprakenMetGebruikteCapaciteit(Collection<Long> teLezenStandplaatsPeriodeIds)
	{
		var criteria = getSession().createCriteria(MammaMammografie.class, "mammografie");
		criteria.createAlias("mammografie.onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode");
		criteria.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");

		criteria.add(Restrictions.lt("afspraak.vanaf", DateUtil.toUtilDate(PlanningConstanten.prognoseVanafDatum)));
		criteria.add(Restrictions.in("standplaatsPeriode.id", teLezenStandplaatsPeriodeIds));
		criteria.add(Restrictions.ge("afspraak.vanaf", DateUtil.toUtilDate(PlanningConstanten.plannenVanafDatum))); 

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("client.id"), "clientId")
			.add(Projections.property("screeningsEenheid.id"), "screeningsEenheidId")
			.add(Projections.property("afspraak.vanaf"), "afspraakMoment")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(AfspraakDaoDto.class));

		return criteria.list();
	}
}
