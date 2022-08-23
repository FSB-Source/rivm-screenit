package nl.rivm.screenit.batch.jobs.mamma.controleuitslag.controlestep;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.mamma.MammaMissendeUitslagenRestrictions;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaControleMissendeUitslagenReader extends BaseScrollableResultReader
{

	private final MammaMissendeUitslagenRestrictions mammaRestrictions;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			var criteria = session.createCriteria(MammaOnderzoek.class, "onderzoek");
			mammaRestrictions.addBeeldenZonderUitslagRestrictions(criteria);
			criteria.addOrder(Order.asc("dossier.id"));
			return criteria;
		}
		catch (Exception e)
		{
			crashMelding("Fout bij het bepalen van missende uitslagen BK", e);
			throw e;
		}
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.distinct(Projections.property("dossier.id"));
	}
}
