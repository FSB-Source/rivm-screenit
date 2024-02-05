package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaScreeningRondeSampleReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var printDatumVoor = DateUtil.toUtilDate(dateSupplier.getLocalDate().minusWeeks(Constants.DEELNAMEKANSBEREKENING_NA_WEKEN));

		var criteria = session.createCriteria(MammaScreeningRonde.class, "screeningRonde");
		criteria.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
		criteria.createAlias("uitnodiging.brief", "brief");
		criteria.createAlias("brief.mergedBrieven", "mergedBrieven", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.isNull("screeningRonde.screeningRondeEvent"));
		criteria.add(Restrictions.or(
			Restrictions.and(
				Restrictions.eq("brief.gegenereerd", true),
				Restrictions.isNull("brief.mergedBrieven"),
				Restrictions.lt("brief.creatieDatum", printDatumVoor)),
			Restrictions.and(
				Restrictions.isNotNull("mergedBrieven.printDatum"),
				Restrictions.lt("mergedBrieven.printDatum", printDatumVoor))));
		criteria.add(Restrictions.ge("screeningRonde.creatieDatum", DateUtil.toUtilDate(dateSupplier.getLocalDate().minusYears(5))));

		return criteria;
	}
}
