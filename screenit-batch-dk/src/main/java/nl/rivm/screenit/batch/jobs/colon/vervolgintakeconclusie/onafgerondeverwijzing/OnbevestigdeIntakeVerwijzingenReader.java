package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.onafgerondeverwijzing;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class OnbevestigdeIntakeVerwijzingenReader extends BaseScrollableResultReader
{

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(ColonIntakeAfspraak.class);
		criteria.createAlias("conclusie", "conclusie");
		criteria.createAlias("nieuweAfspraak", "nieuweAfspraak");

		criteria.add(Restrictions.eq("conclusie.type", ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM));
		criteria.add(Restrictions.eq("conclusie.doorverwijzingBevestigd", Boolean.FALSE));
		criteria.add(Restrictions.lt("nieuweAfspraak.vanaf", currentDateSupplier.getLocalDateTime().plusDays(1)));
		return criteria;
	}
}
