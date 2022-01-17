package nl.rivm.screenit.batch.jobs.cervix.heroverwegers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixHeroverwegersReader extends BaseScrollableResultReader
{

	@Autowired
	private CervixSelectieRestrictionsService selectieRestrictionsService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(CervixCISHistorie.class);

		ExecutionContext stepContext = getStepExecutionContext();
		Long bmhkLabId = (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB);

		criteria.createAlias("dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		criteria.createAlias("adres.gbaGemeente", "gemeente");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		selectieRestrictionsService.addClientSelectieRestrictions(criteria, getMaxAantalWekenVertraging(bmhkLabId) * 7);

		criteria.add(Restrictions.isNotNull("gemeente.screeningOrganisatie"));
		criteria.add(Restrictions.eq("gemeente.bmhkLaboratorium.id", bmhkLabId));

		criteria.createAlias("afmelding", "afmelding");

		criteria.add(Restrictions.eqProperty("afmelding", "dossier.laatsteAfmelding"));

		criteria.add(Restrictions.eq("afmelding.reden", CervixAfmeldingReden.ANDERS));

		criteria.add(Restrictions.isEmpty("afmelding.brieven"));

		criteria.add(Restrictions.eq("persoon.geslacht", Geslacht.VROUW));

		return criteria;
	}

	private Integer getMaxAantalWekenVertraging(Long bmhkLabId)
	{
		BMHKLaboratorium bmhkLab = hibernateService.get(BMHKLaboratorium.class, bmhkLabId);
		return instellingService.getOrganisatieParameter(bmhkLab, OrganisatieParameterKey.CERVIX_MAX_AANTAL_HEROVERWEGERS, 0);
	}

}
