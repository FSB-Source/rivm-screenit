package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaBaseBeoordelingDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
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
public class MammaBaseBeoordelingDaoImpl extends AbstractAutowiredDao implements MammaBaseBeoordelingDao
{
	@Override
	public List<MammaBeoordeling> getVrijTeGevenBeoordelingen(InstellingGebruiker ingelogdeGebruiker)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class);
		criteria.add(Restrictions.eq("reserveringhouder", ingelogdeGebruiker));
		criteria.add(Restrictions.not(Restrictions.in("status", Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN))));
		return criteria.list();
	}

	@Override
	public MammaBeoordeling getBeoordelingVanLezing(MammaLezing lezing)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class);
		criteria.add(Restrictions.or(
			Restrictions.eq("eersteLezing", lezing),
			Restrictions.eq("tweedeLezing", lezing),
			Restrictions.eq("discrepantieLezing", lezing),
			Restrictions.eq("arbitrageLezing", lezing),
			Restrictions.eq("verslagLezing", lezing)));
		return (MammaBeoordeling) criteria.uniqueResult();
	}

	@Override
	public MammaScreeningRonde getScreeningrondeVoorFollowUp(Client client, Date onderzoeksdatum)
	{
		Criteria crit = getSession().createCriteria(MammaBeoordeling.class);
		crit.createAlias("onderzoek", "onderzoek");
		crit.createAlias("onderzoek.afspraak", "afspraak");
		crit.createAlias("afspraak.uitnodiging", "uitnodiging");
		crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		crit.createAlias("screeningRonde.dossier", "dossier");
		crit.createAlias("dossier.client", "client");
		crit.add(Restrictions.eq("client.id", client.getId()));
		crit.add(Restrictions.in("status", MammaBeoordelingStatus.uitslagStatussen()));
		if (onderzoeksdatum != null)
		{
			crit.add(Restrictions.lt("statusDatum", onderzoeksdatum));
		}
		crit.addOrder(Order.desc("statusDatum"));
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

	@Override
	public MammaBeoordeling getLaatsteBeoordelingMetUitslag(MammaDossier dossier)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class, "beoordeling");
		criteria.createAlias("beoordeling.onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");

		criteria.add(Restrictions.eq("dossier.id", dossier.getId()));
		criteria.add(Restrictions.in("beoordeling.status", MammaBeoordelingStatus.uitslagStatussen()));

		criteria.addOrder(Order.desc("beoordeling.statusDatum"));
		criteria.setMaxResults(1);
		return (MammaBeoordeling) criteria.uniqueResult();
	}
}
