package nl.rivm.screenit.batch.jobs.mamma.aftergba.nieuwepostcodes;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSqlScrollableResultReader;

import org.hibernate.HibernateException;
import org.hibernate.SQLQuery;
import org.hibernate.StatelessSession;
import org.hibernate.type.StandardBasicTypes;

public class MammaNieuwePostcodesReader extends BaseSqlScrollableResultReader
{

	@Override
	public SQLQuery createCriteria(StatelessSession session) throws HibernateException
	{
		SQLQuery criteria = session.createSQLQuery("select c.id "
			+ "from org_adres a "
			+ "inner join pat_persoon pc on pc.gba_adres = a.id "
			+ "inner join pat_patient c on c.id = pc.patient "
			+ "inner join algemeen.gemeente g on g.id = a.gba_gemeente "

			+ "left outer join mamma.postcode_reeks r on (a.postcode >= r.van_postcode and a.postcode <= r.tot_postcode) "
			+ "where a.dtype = 'BagAdres' "
			+ "and r.id is null "
			+ "and a.postcode is not null "
			+ "and g.screening_organisatie is not null "
			+ "and pc.datum_vertrokken_uit_nederland is null "
			+ "and pc.overlijdensdatum is null "
			+ "and c.gba_status = 'INDICATIE_AANWEZIG'"
			+ "and pc.geslacht = 'VROUW'"
			+ "and c.mamma_dossier is not null");
		criteria.addScalar("id", StandardBasicTypes.LONG);
		return criteria;
	}
}
