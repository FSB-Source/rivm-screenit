package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Property;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class LabformulierGenererenReader extends BaseScrollableResultReader
{

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var crit = session.createCriteria(CervixLabformulierAanvraag.class);
		crit.createAlias("huisartsLocatie", "locatie");

		crit.createAlias("locatie.huisarts", "huisarts");

		crit.add(Restrictions.eq("status", CervixLabformulierAanvraagStatus.AANGEVRAAGD));

		crit.add(Restrictions.eq("huisarts.actief", Boolean.TRUE));

		crit.add(Restrictions.ne("locatie.status", CervixLocatieStatus.INACTIEF));

		var uitstrijkendeArtsSubQuery = DetachedCriteria.forClass(CervixHuisartsLocatie.class);
		uitstrijkendeArtsSubQuery.createAlias("locatieAdres", "locatieAdres", JoinType.INNER_JOIN);
		uitstrijkendeArtsSubQuery.createAlias("locatieAdres.gbaGemeente", "gemeente", JoinType.INNER_JOIN);
		uitstrijkendeArtsSubQuery.createAlias("gemeente.screeningOrganisatie", "screeningOrganisatie", JoinType.INNER_JOIN);
		uitstrijkendeArtsSubQuery.add(Restrictions.eq("screeningOrganisatie.id", getScreeningOrganisatieId()));
		uitstrijkendeArtsSubQuery.setProjection(Property.forName("id"));
		crit.add(Subqueries.propertyIn("locatie.id", uitstrijkendeArtsSubQuery));

		return crit;
	}

	private Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(LabformulierGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}
}
