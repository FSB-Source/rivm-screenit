package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.cervixlabformuliersphereonverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.heeftScanDatumTotEnMet;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.isDigitaal;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.isNietGewist;

@Component
@AllArgsConstructor
public class CervixOudeLabformulierenVerwijderenReader extends BaseSpecificationScrollableResultReader<CervixLabformulier>
{
	private final ICurrentDateSupplier dateSupplier;

	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var minVerwijderenDatum = DateUtil.toUtilDate(dateSupplier.getLocalDate().minusYears(1));

		var criteria = session.createCriteria(CervixLabformulier.class);

		criteria.add(Restrictions.le("scanDatum", minVerwijderenDatum));
		criteria.add(Restrictions.eq("digitaal", false));
		criteria.add(Restrictions.isNull("datumGewist"));
		criteria.setMaxResults(100000);

		return criteria;
	}

	@Override
	protected Specification<CervixLabformulier> createSpecification()
	{
		var minVerwijderenDatum = DateUtil.toUtilDate(dateSupplier.getLocalDate().minusYears(1));
		return heeftScanDatumTotEnMet(minVerwijderenDatum)
			.and(isDigitaal(false))
			.and(isNietGewist());
	}
}
