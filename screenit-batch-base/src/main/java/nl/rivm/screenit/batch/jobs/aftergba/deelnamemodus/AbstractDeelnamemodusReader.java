package nl.rivm.screenit.batch.jobs.aftergba.deelnamemodus;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;

public abstract class AbstractDeelnamemodusReader extends BaseScrollableResultReader
{
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(dossierClass(), "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		var manOfOnbekendMetDeelnamemodusStandaard = Restrictions.and(
			Restrictions.in("persoon.geslacht", Geslacht.MAN, Geslacht.ONBEKEND),
			Restrictions.eq("dossier.deelnamemodus", Deelnamemodus.STANDAARD));

		var vrouwMetSelectieblokkade = Restrictions.and(
			Restrictions.eq("persoon.geslacht", Geslacht.VROUW),
			Restrictions.eq("dossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE));

		criteria.add(Restrictions.or(manOfOnbekendMetDeelnamemodusStandaard, vrouwMetSelectieblokkade));

		return criteria;
	}

	protected abstract Class<?> dossierClass();
}
