package nl.rivm.screenit.batch.jobs.colon.selectie.uitnodingingpushstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Date;

import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public class UitnodigingU2PushReader extends AbstractUitnodigingPushReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	public UitnodigingU2PushReader()
	{
		super(ColonUitnodigingCategorie.U2);
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Date vandaag = currentDateSupplier.getDateMidnight();

		Criteria crit = session.createCriteria(ProjectClient.class);
		crit.createAlias("project", "project", JoinType.INNER_JOIN);
		crit.createAlias("groep", "groep", JoinType.INNER_JOIN);
		crit.createAlias("client", "client", JoinType.INNER_JOIN);
		crit.createAlias("client.persoon", "persoon", JoinType.INNER_JOIN);
		crit.createAlias("persoon.gbaAdres", "adres", JoinType.INNER_JOIN);
		crit.createAlias("adres.gbaGemeente", "gemeente", JoinType.INNER_JOIN);
		crit.createAlias("gemeente.screeningOrganisatie", "screeningorganisatie", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("client.colonDossier", "dossier", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("volgendeUitnodiging.interval", "interval", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(ColonRestrictions.getU2BaseCriteria(currentDateSupplier.getLocalDate()));

		crit.add(Restrictions.eq("isUitgenodigdInProjectPeriode", Boolean.FALSE));

		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(Restrictions.eq("groep.actief", Boolean.TRUE));
		crit.add(Restrictions.and(
			Restrictions.gt("project.eindDatum", vandaag), 
			Restrictions.le("project.startDatum", vandaag)
		)); 

		crit.add(Restrictions.eq("groep.uitnodigingenPushenNa", vandaag));

		return crit;
	}
}
