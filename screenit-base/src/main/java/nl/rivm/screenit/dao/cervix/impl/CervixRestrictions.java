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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixRestrictions
{

	public static void addMissendeUitslagRestrictions(Criteria criteria, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		criteria.createAlias("monster.ontvangstScreeningRonde", "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		criteria.add(Restrictions.le("monster.ontvangstdatum", DateUtil.toUtilDate(minimaleSignaleringsDatum)));

		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.gt("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
					DateRestrictions.gtProperty("monster.ontvangstdatum", "dossier.datumLaatstGecontroleerdeSignalering")
				),
				Restrictions.and(
					Restrictions.or(
						DateRestrictions.le("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
						Restrictions.isNull("dossier.datumLaatstGecontroleerdeSignalering")
					),
					Restrictions.gt("monster.ontvangstdatum", DateUtil.toUtilDate(signalerenVanaf))
				)
			)
		);

		var uitslagBriefQuery = maakUitslagBriefVanMonsterQuery();

		criteria.add(Restrictions.or(
			Restrictions.and(
				Restrictions.ne("uitstrijkjeStatus", CervixUitstrijkjeStatus.NIET_ONTVANGEN),
				Subqueries.notExists(uitslagBriefQuery)
			),
			Restrictions.and(
				Restrictions.ne("zasStatus", CervixZasStatus.VERSTUURD),
				Subqueries.notExists(uitslagBriefQuery)
			),
			Restrictions.and(
				Restrictions.eq("zasStatus", CervixZasStatus.NIET_ANALYSEERBAAR),
				Subqueries.notExists(maakNieuweUitnodigingZasZelfdeRondeQuery())
			)));

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
	}

	private static DetachedCriteria maakNieuweUitnodigingZasZelfdeRondeQuery()
	{
		var criteria = DetachedCriteria.forClass(CervixUitnodiging.class, "uitnodiging");
		criteria.setProjection(Projections.id());
		criteria.createAlias("uitnodiging.brief", "brief");
		criteria.add(Restrictions.eq("uitnodiging.monsterType", CervixMonsterType.ZAS));
		criteria.add(Restrictions.eqProperty("uitnodiging.screeningRonde", "ronde.id"));
		criteria.add(DateRestrictions.geProperty("uitnodiging.creatieDatum", "monster.statusDatum"));
		criteria.add(Restrictions.eq("brief.gegenereerd", true));
		return criteria;
	}

	private static DetachedCriteria maakUitslagBriefVanMonsterQuery()
	{
		var criteria = DetachedCriteria.forClass(CervixBrief.class, "brief");
		criteria.setProjection(Projections.id());
		criteria.createAlias("brief.projectBrief", "projectBrief", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("brief.monster", "briefMonster");
		criteria.add(Restrictions.eqProperty("briefMonster.id", "monster.id"));
		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", false),
					Restrictions.in("brief.briefType", BriefType.CERVIX_UITSLAG_BRIEVEN),
					Restrictions.eq("brief.gegenereerd", true)
				),
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", true),
					Restrictions.in("projectBrief.briefType", BriefType.CERVIX_UITSLAG_BRIEVEN),
					Restrictions.eq("projectBrief.gegenereerd", true)
				)
			)
		);
		return criteria;
	}
}
