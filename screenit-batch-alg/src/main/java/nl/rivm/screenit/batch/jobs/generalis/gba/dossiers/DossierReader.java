package nl.rivm.screenit.batch.jobs.generalis.gba.dossiers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.Client;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;

public class DossierReader extends BaseScrollableResultReader
{
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(Client.class, "client");
		crit.createAlias("client.persoon", "persoon");

		crit.add(Restrictions.or(
			Restrictions.isNull("client.colonDossier"),
			Restrictions.and(
				Restrictions.eq("persoon.geslacht", Geslacht.VROUW),
				Restrictions.or(
					Restrictions.isNull("client.cervixDossier"),
					Restrictions.isNull("client.mammaDossier")))));

		return crit;
	}
}
