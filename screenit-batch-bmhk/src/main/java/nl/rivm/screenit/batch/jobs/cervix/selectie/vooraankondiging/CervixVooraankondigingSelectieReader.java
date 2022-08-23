package nl.rivm.screenit.batch.jobs.cervix.selectie.vooraankondiging;

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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
public class CervixVooraankondigingSelectieReader extends BaseScrollableResultReader
{

	private final CervixSelectieRestrictionsService selectieRestrictionsService;

	private final InstellingService instellingService;

	private final HibernateService hibernateService;

	public CervixVooraankondigingSelectieReader(CervixSelectieRestrictionsService selectieRestrictionsService, InstellingService instellingService,
		HibernateService hibernateService)
	{
		super.setFetchSize(50);
		this.selectieRestrictionsService = selectieRestrictionsService;
		this.instellingService = instellingService;
		this.hibernateService = hibernateService;
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var stepContext = getStepExecutionContext();
		Long bmhkLabId = (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB);

		var crit = session.createCriteria(Client.class, "rootClient");
		crit.createAlias("rootClient.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("adres.gbaGemeente", "gemeente");
		crit.createAlias("rootClient.cervixDossier", "dossier");

		selectieRestrictionsService.addVooraankondigingSelectieRestrictions(crit);
		ScreenitRestrictions.addClientBaseRestrictions(crit, "rootClient", "persoon");

		crit.add(Restrictions.isNotNull("gemeente.screeningOrganisatie"));
		crit.add(Restrictions.eq("gemeente.bmhkLaboratorium.id", bmhkLabId));

		crit.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));
		crit.add(Restrictions.eq("dossier.wachtOpStartProject", false));
		crit.add(Restrictions.ne("dossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE));

		return crit;
	}
}
