package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaBaseScreeningrondeDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseScreeningrondeDaoImpl extends AbstractAutowiredDao implements MammaBaseScreeningrondeDao
{
	@Override
	public MammaScreeningRonde getLaatsteScreeningRondeMetUitslag(Client client, Date voorDatum)
	{
		return getLaatsteScreeningRondeMetBeoordelingStatus(client, MammaBeoordelingStatus.uitslagStatussen(), voorDatum);
	}

	@Override
	public MammaScreeningRonde getLaatsteScreeningRondeMetPositieveUitslag(Client client, Date voorDatum)
	{
		return getLaatsteScreeningRondeMetBeoordelingStatus(client, Collections.singletonList(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG), voorDatum);
	}

	private MammaScreeningRonde getLaatsteScreeningRondeMetBeoordelingStatus(Client client, List<MammaBeoordelingStatus> statuses, Date voorDatum)
	{
		Criteria crit = getSession().createCriteria(MammaScreeningRonde.class, "screeningRonde");
		crit.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
		crit.createAlias("uitnodiging.afspraken", "afspraak");
		crit.createAlias("afspraak.onderzoek", "onderzoek");
		crit.createAlias("onderzoek.beoordelingen", "beoordeling");
		crit.createAlias("screeningRonde.dossier", "dossier1");
		crit.createAlias("dossier1.client", "client");
		crit.add(Restrictions.eq("client.id", client.getId()));
		crit.add(Restrictions.in("beoordeling.status", statuses));
		if (voorDatum != null)
		{
			crit.add(Restrictions.lt("beoordeling.statusDatum", voorDatum));
		}
		crit.addOrder(Order.desc("beoordeling.statusDatum"));
		crit.setProjection(Projections.property("screeningRonde.id"));
		crit.setMaxResults(1);
		Long screeningRondeId = (Long) crit.uniqueResult();
		MammaScreeningRonde screeningRonde = null;
		if (screeningRondeId != null)
		{
			screeningRonde = getSession().get(MammaScreeningRonde.class, screeningRondeId);
		}

		return screeningRonde;
	}
}
