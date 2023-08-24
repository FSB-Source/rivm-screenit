package nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep;

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

import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.rivm.screenit.util.query.SmartSQLProjection;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
public class CervixSelectieReader extends BaseScrollableResultReader
{

	private final CervixSelectieRestrictionsService selectieRestrictionsService;

	private final OrganisatieParameterService organisatieParameterService;

	private final HibernateService hibernateService;

	public CervixSelectieReader(CervixSelectieRestrictionsService selectieRestrictionsService, OrganisatieParameterService organisatieParameterService,
		HibernateService hibernateService)
	{
		super.setFetchSize(50);
		this.selectieRestrictionsService = selectieRestrictionsService;
		this.organisatieParameterService = organisatieParameterService;
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

		selectieRestrictionsService.addClientSelectieRestrictions(crit);
		ScreenitRestrictions.addClientBaseRestrictions(crit, "rootClient", "persoon");

		crit.add(Restrictions.isNotNull("gemeente.screeningOrganisatie"));
		crit.add(Restrictions.eq("gemeente.bmhkLaboratorium.id", bmhkLabId));

		crit.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));
		crit.add(Restrictions.eq("dossier.wachtOpStartProject", false));
		crit.add(Restrictions.ne("dossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE));

		Integer maxAantalClienten = getMaxAantalClienten();
		if (maxAantalClienten != null)
		{
			crit.setMaxResults(maxAantalClienten);
			crit.addOrder(Order.asc("geboortedag"));
		}

		return crit;
	}

	private Integer getMaxAantalClienten()
	{
		var stepContext = getStepExecutionContext();
		var bmhkLab = hibernateService.get(BMHKLaboratorium.class, (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB));
		return organisatieParameterService.getOrganisatieParameter(bmhkLab, OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_SELECTIE);
	}

	@Override
	protected Projection getProjection()
	{
		Integer maxAantalClienten = getMaxAantalClienten();
		if (maxAantalClienten != null)
		{
			return Projections.distinct(
				Projections.projectionList()
					.add(Projections.id())
					.add(Projections.alias(
						new SmartSQLProjection(

							"to_char({persoon}.geboortedatum + interval '1 day' * ((date_trunc('YEAR', CURRENT_DATE + interval '1 year')::DATE  - interval '1' day)::date - CURRENT_DATE::date), 'DDD') as geboortedag",
							new String[] { "geboortedag" },
							new org.hibernate.type.IntegerType[] { new org.hibernate.type.IntegerType() }),
						"geboortedag")));
		}
		return super.getProjection();
	}
}
