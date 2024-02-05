package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixVerlateDeelnameCovidReader extends BaseScrollableResultReader
{
	private final OrganisatieParameterService organisatieParameterService;

	private final HibernateService hibernateService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var stepContext = getStepExecutionContext();
		Long bmhkLabId = (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB);

		var crit = session.createCriteria(Client.class, "client");
		crit.createAlias("client.cervixDossier", "dossier");
		crit.createAlias("dossier.laatsteScreeningRonde", "ronde");
		crit.createAlias("ronde.laatsteUitnodiging", "uitnodiging");
		crit.createAlias("uitnodiging.monster", "monster");
		crit.createAlias("uitnodiging.brief", "brief");
		crit.createAlias("ronde.uitstel", "uitstel", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("adres.gbaGemeente", "gemeente");

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.isNotNull("gemeente.screeningOrganisatie"));
		crit.add(Restrictions.eq("gemeente.bmhkLaboratorium.id", bmhkLabId));

		crit.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.LOPEND));
		crit.add(Restrictions.eq("ronde.aangemeld", true));

		crit.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));
		crit.add(Restrictions.eq("dossier.aangemeld", true));

		crit.add(Restrictions.isNull("uitstel.uitstellenTotDatum"));

		crit.add(Restrictions.eq("uitnodiging.monsterType", CervixMonsterType.UITSTRIJKJE));

		crit.add(Restrictions.eq("brief.gegenereerd", true));
		crit.add(Restrictions.between("brief.creatieDatum",
			DateUtil.parseDateForPattern("01-10-2019", Constants.DEFAULT_DATE_FORMAT),
			DateUtil.parseDateForPattern("01-01-2022", Constants.DEFAULT_DATE_FORMAT)));

		var subquery = DetachedCriteria.forClass(CervixMonster.class, "monsterSub");
		subquery.add(Restrictions.eqProperty("monsterSub.ontvangstScreeningRonde", "brief.screeningRonde"));
		subquery.add(Restrictions.gtProperty("monsterSub.ontvangstdatum", "brief.creatieDatum"));
		subquery.setProjection(Projections.id());
		crit.add(Subqueries.notExists(subquery));

		crit.setMaxResults(getMaxAantalClienten(bmhkLabId));
		crit.addOrder(Order.asc("brief.creatieDatum"));

		return crit;
	}

	private Integer getMaxAantalClienten(Long bmhkLabId)
	{
		var bmhkLab = hibernateService.get(BMHKLaboratorium.class, bmhkLabId);
		return organisatieParameterService.getOrganisatieParameter(bmhkLab, OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_VERLATE_DEELNAME);
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.distinct(Projections.projectionList().add(Projections.property("brief.id")).add(Projections.property("brief.creatieDatum")));
	}
}
