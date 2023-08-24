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

import java.util.List;

import nl.rivm.screenit.mamma.planning.dao.PlanningBlokkadeDao;
import nl.rivm.screenit.mamma.planning.dao.dto.BlokkadeDaoDto;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningBlokkadeDaoImpl extends AbstractAutowiredDao implements PlanningBlokkadeDao
{
	@Override
	public List<BlokkadeDaoDto> actieveBlokkadesVoorConceptmodel()
	{
		var criteria = getSession().createCriteria(MammaBlokkade.class, "blokkade");

		criteria.add(Restrictions.eq("blokkade.actief", true));
		criteria.add(Restrictions.ge("blokkade.totEnMet", DateUtil.toUtilDate(PlanningConstanten.plannenVanafDatum)));
		criteria.add(Restrictions.le("blokkade.vanaf", DateUtil.toUtilDate(PlanningConstanten.plannenTotEnMetDatum)));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("blokkade.id"), "id")
			.add(Projections.property("blokkade.type"), "type")
			.add(Projections.property("blokkade.standplaats.id"), "standplaatsId")
			.add(Projections.property("blokkade.screeningsEenheid.id"), "screeningsEenheidId")
			.add(Projections.property("blokkade.regio.id"), "screeningOrganisatieId")
			.add(Projections.property("blokkade.vanaf"), "vanaf")
			.add(Projections.property("blokkade.totEnMet"), "totEnMet")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(BlokkadeDaoDto.class));

		return criteria.list();
	}
}
