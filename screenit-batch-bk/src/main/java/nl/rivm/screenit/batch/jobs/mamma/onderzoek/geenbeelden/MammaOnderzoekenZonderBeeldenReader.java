package nl.rivm.screenit.batch.jobs.mamma.onderzoek.geenbeelden;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
public class MammaOnderzoekenZonderBeeldenReader extends BaseScrollableResultReader
{

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("dossier.laatsteScreeningRonde", "ronde");
		criteria.createAlias("ronde.laatsteUitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.laatsteAfspraak", "afspraak");
		criteria.createAlias("afspraak.onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.mammografie", "mammografie", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.or(
			Restrictions.eq("onderzoek.status", MammaOnderzoekStatus.AFGEROND),
			Restrictions.and(
				Restrictions.eq("onderzoek.status", MammaOnderzoekStatus.ONVOLLEDIG),
				Restrictions.eq("onderzoek.onvolledigOnderzoek", OnvolledigOnderzoekOption.MET_FOTOS))));
		criteria.add(Restrictions.or(
			Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.NIET_BESCHIKBAAR),
			Restrictions.isNull("onderzoek.mammografie")));

		return criteria;
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.property("afspraak.id");
	}
}
