package nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.step;

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

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.MammaBaseBeoordelingReader;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
public class MammaBeoordelingenAccorderenReader extends MammaBaseBeoordelingReader
{

	private final static int DAGEN = 1;

	private final static int UREN = 2;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(MammaBeoordeling.class, "beoordeling");

		var radiologenAanHetWerkSubquery = getRadiologenDieAanHetWerkZijn(UREN);

		criteria.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);

		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN),
					Subqueries.propertyNotIn("eersteLezing.beoordelaar.id", radiologenAanHetWerkSubquery)),
				Restrictions.and(
					Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN),
					Subqueries.propertyNotIn("tweedeLezing.beoordelaar.id", radiologenAanHetWerkSubquery)),
				Restrictions.and(
					Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN),
					Restrictions.le("eersteLezing.beoordelingDatum", DateUtil.minDagen(currentDateSupplier.getDate(), DAGEN)),
					Restrictions.and(
						Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN),
						Restrictions.le("tweedeLezing.beoordelingDatum", DateUtil.minDagen(currentDateSupplier.getDate(), DAGEN))))));

		return criteria;
	}

}
