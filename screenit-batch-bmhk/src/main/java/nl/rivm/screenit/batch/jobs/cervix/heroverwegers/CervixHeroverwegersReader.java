package nl.rivm.screenit.batch.jobs.cervix.heroverwegers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixHeroverwegersReader extends BaseScrollableResultReader
{

	@Autowired
	private CervixSelectieRestrictionsService selectieRestrictionsService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(CervixCISHistorie.class);
		criteria.createAlias("dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		selectieRestrictionsService.addClientSelectieRestrictions(criteria);

		criteria.createAlias("afmelding", "afmelding");

		criteria.add(Restrictions.eqProperty("afmelding", "dossier.laatsteAfmelding"));

		criteria.add(Restrictions.eq("afmelding.reden", CervixAfmeldingReden.ANDERS));

		criteria.add(Restrictions.isEmpty("afmelding.brieven"));

		criteria.add(Restrictions.eq("persoon.geslacht", Geslacht.VROUW));

		return criteria;
	}
}
