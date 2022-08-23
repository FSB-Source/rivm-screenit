package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.rondefactor;

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
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaEersteOnderzoekBijwerkenReader extends BaseScrollableResultReader
{

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var actieveStandplaatsRondes = DetachedCriteria.forClass(MammaStandplaatsRonde.class, "standplaatsRonde");
		actieveStandplaatsRondes.createAlias("standplaatsRonde.standplaatsPerioden", "standplaatsPeriodes");
		actieveStandplaatsRondes.add(Restrictions.ge("standplaatsPeriodes.totEnMet", currentDateSupplier.getDateMidnight()));
		actieveStandplaatsRondes.setProjection(Projections.property("standplaatsRonde.id"));

		var criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.screeningRondes", "rondes");
		criteria.createAlias("rondes.laatsteOnderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("onderzoek.mammografie", "mammografie");
		criteria.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode");
		criteria.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");

		criteria.add(Restrictions.eq("dossier.eersteOnderzoek", true));
		criteria.add(Restrictions.ne("mammografie.ilmStatus", MammaMammografieIlmStatus.NIET_BESCHIKBAAR));
		criteria.add(Subqueries.propertyNotIn("standplaatsRonde.id", actieveStandplaatsRondes));

		return criteria;
	}

}
