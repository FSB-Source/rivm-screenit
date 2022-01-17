package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaStandplaatsPeriodeDao;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaStandplaatsPeriodeDaoImpl extends AbstractAutowiredDao implements MammaStandplaatsPeriodeDao
{
	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPeriodesVoorBulkVerzetten(ScreeningOrganisatie regio, Date verzettenVanaf)
	{
		Criteria crit = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");
		crit.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit.createAlias("standplaatsRonde.standplaats", "standplaats");
		crit.createAlias("standplaats.regio", "regio");
		crit.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid");
		crit.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		crit.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
		crit.createAlias("centraleEenheid.regio", "ceRegio");
		crit.add(Restrictions.eq("ceRegio.id", regio.getId()));
		crit.add(Restrictions.eq("regio.id", regio.getId()));
		crit.add(Restrictions.geProperty("screeningsEenheid.vrijgegevenTotEnMet", "standplaatsPeriode.vanaf"));
		crit.add(Restrictions.ge("screeningsEenheid.vrijgegevenTotEnMet", verzettenVanaf));
		crit.add(Restrictions.ge("standplaatsPeriode.totEnMet", verzettenVanaf));
		crit.addOrder(Order.asc("screeningsEenheid.naam"));
		crit.addOrder(Order.asc("standplaats.naam"));

		return crit.list();
	}

	@Override
	public MammaStandplaatsRondeUitnodigenRapportage getStandplaatsRondeUitnodigenRapportage(MammaStandplaatsRonde standplaatsRonde)
	{
		Criteria crit = getSession().createCriteria(MammaStandplaatsRondeUitnodigenRapportage.class, "standplaatsRondeUitnodigenRapportage");
		crit.createAlias("standplaatsRondeUitnodigenRapportage.uitnodigenRapportage", "uitnodigenRapportage");
		crit.add(Restrictions.eq("standplaatsRondeUitnodigenRapportage.standplaatsRonde.id", standplaatsRonde.getId()));
		crit.addOrder(Order.desc("uitnodigenRapportage.datumVerwerking"));
		crit.setMaxResults(1);
		return (MammaStandplaatsRondeUitnodigenRapportage) crit.uniqueResult();
	}
}
