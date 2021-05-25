package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseAfspraakDaoImpl extends AbstractAutowiredDao implements MammaBaseAfspraakDao
{
	@Override
	public List<MammaAfspraak> getNietGekoppeldeAfspraken(MammaCapaciteitBlok capaciteitsBlok)
	{
		Criteria crit = createAfsprakenCriteria(capaciteitsBlok.getScreeningsEenheid(), false);
		afspraakStatussen(crit, MammaAfspraakStatus.GEPLAND);
		vanafTot(crit, capaciteitsBlok.getVanaf(), capaciteitsBlok.getTot());

		crit.createAlias("uitnodiging.screeningRonde", "screeningronde");
		crit.createAlias("screeningronde.dossier", "dossier");

		crit.add(Restrictions.isNull("afspraak.capaciteitBlok"));
		MammaCapaciteitBlokType blokType = capaciteitsBlok.getBlokType();
		if (!blokType.equals(MammaCapaciteitBlokType.TEHUIS))
		{
			crit.add(Restrictions.in("dossier.doelgroep", blokType.getDoelgroepen()));
			crit.add(Restrictions.isNull("dossier.tehuis"));
		}
		else
		{
			crit.add(Restrictions.isNotNull("dossier.tehuis"));
		}
		return crit.list();
	}

	@Override
	public List<MammaAfspraak> getAfspraken(MammaScreeningsEenheid screeningsEenheid, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return createAfsprakenCriteria(screeningsEenheid, vanaf, totEnMet, true, afspraakStatussen).list();
	}

	@Override
	public List<MammaAfspraak> getAfspraken(String seCode, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return createAfsprakenCriteria(seCode, vanaf, totEnMet, false, afspraakStatussen).list();
	}

	@Override
	public Date[] getEersteEnLaatsteAfspraakDatum(long standplaatsPeriodeId, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria afsprakenCriteria = createAfsprakenCriteria(standplaatsPeriodeId, vanaf, totEnMet, false, afspraakStatussen);
		afsprakenCriteria.setProjection(Projections.projectionList().add(Projections.min("afspraak.vanaf")).add(Projections.max("afspraak.vanaf")));
		Object[] result = (Object[]) afsprakenCriteria.uniqueResult();
		return new Date[] { (Date) result[0], (Date) result[1] };
	}

	@Override
	public long countAfspraken(MammaStandplaats standplaats, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return count(createAfsprakenCriteria(standplaats, vanaf, totEnMet, false, afspraakStatussen));
	}

	@Override
	public List<MammaAfspraak> getAfspraken(MammaStandplaats standplaats, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return createAfsprakenCriteria(standplaats, vanaf, totEnMet, false, afspraakStatussen).list();
	}

	@Override
	public long countAfspraken(long standplaatsPeriodeId, MammaAfspraakStatus... afspraakStatussen)
	{
		return count(createAfsprakenCriteria(standplaatsPeriodeId, false, afspraakStatussen));
	}

	@Override
	public long countAfspraken(MammaScreeningsEenheid screeningsEenheid, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return count(createAfsprakenCriteria(screeningsEenheid, vanaf, totEnMet, false, afspraakStatussen));
	}

	private long count(Criteria crit)
	{
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	private Criteria createAfsprakenCriteria(MammaScreeningsEenheid screeningsEenheid, Date vanaf, Date totEnMet, boolean bepaalBenodigdeCapaciteit,
		MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria crit = createAfsprakenCriteria(screeningsEenheid, bepaalBenodigdeCapaciteit);
		vanafTotEnMet(crit, vanaf, totEnMet);
		afspraakStatussen(crit, afspraakStatussen);
		return crit;
	}

	private Criteria createAfsprakenCriteria(String seCode, Date vanaf, Date totEnMet, boolean bepaalBenodigdeCapaciteit, MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria crit = createAfsprakenCriteria(seCode, bepaalBenodigdeCapaciteit);
		vanafTotEnMet(crit, vanaf, totEnMet);
		afspraakStatussen(crit, afspraakStatussen);
		return crit;
	}

	private Criteria createAfsprakenCriteria(MammaStandplaats standplaats, Date vanaf, Date totEnMet, boolean bepaalBenodigdeCapaciteit, MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria crit = createAfsprakenCriteria(standplaats, bepaalBenodigdeCapaciteit);
		vanafTotEnMet(crit, vanaf, totEnMet);
		afspraakStatussen(crit, afspraakStatussen);
		return crit;
	}

	private Criteria createAfsprakenCriteria(long standplaatsPeriodeId, boolean bepaalBenodigdeCapaciteit, MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria crit = createAfsprakenCriteria(standplaatsPeriodeId, bepaalBenodigdeCapaciteit);
		afspraakStatussen(crit, afspraakStatussen);
		return crit;
	}

	private Criteria createAfsprakenCriteria(long standplaatsPeriodeId, Date vanaf, Date totEnMet, boolean bepaalBenodigdeCapaciteit, MammaAfspraakStatus... afspraakStatussen)
	{
		Criteria crit = createAfsprakenCriteria(standplaatsPeriodeId, bepaalBenodigdeCapaciteit);
		vanafTotEnMet(crit, vanaf, totEnMet);
		afspraakStatussen(crit, afspraakStatussen);
		return crit;
	}

	private void vanafTotEnMet(Criteria crit, Date vanaf, Date totEnMet)
	{
		if (vanaf != null)
		{
			crit.add(Restrictions.ge("afspraak.vanaf", vanaf));
		}
		if (totEnMet != null)
		{
			crit.add(Restrictions.le("afspraak.vanaf", totEnMet));
		}
	}

	private void vanafTot(Criteria crit, Date vanaf, Date tot)
	{
		if (vanaf != null)
		{
			crit.add(Restrictions.ge("afspraak.vanaf", vanaf));
		}
		if (tot != null)
		{
			crit.add(Restrictions.lt("afspraak.vanaf", tot));
		}
	}

	private void afspraakStatussen(Criteria crit, MammaAfspraakStatus... afspraakStatussen)
	{
		if (afspraakStatussen != null)
		{
			crit.add(Restrictions.in("afspraak.status", afspraakStatussen));
		}
	}

	private Criteria createAfsprakenCriteria(MammaScreeningsEenheid screeningsEenheid, boolean bepaalBenodigdeCapaciteit)
	{
		Criteria crit = createAfsprakenCriteria(bepaalBenodigdeCapaciteit);
		crit.add(Restrictions.eq("standplaatsPeriode.screeningsEenheid", screeningsEenheid));
		return crit;
	}

	private Criteria createAfsprakenCriteria(String seCode, boolean bepaalBenodigdeCapaciteit)
	{
		Criteria crit = createAfsprakenCriteria(bepaalBenodigdeCapaciteit);
		crit.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid");
		crit.add(Restrictions.eq("screeningsEenheid.code", seCode));
		return crit;
	}

	private Criteria createAfsprakenCriteria(long standplaatsPeriodeId, boolean bepaalBenodigdeCapaciteit)
	{
		Criteria crit = createAfsprakenCriteria(bepaalBenodigdeCapaciteit);
		crit.add(Restrictions.eq("standplaatsPeriode.id", standplaatsPeriodeId));
		return crit;
	}

	private Criteria createAfsprakenCriteria(MammaStandplaats standplaats, boolean bepaalBenodigdeCapaciteit)
	{
		Criteria crit = createAfsprakenCriteria(bepaalBenodigdeCapaciteit);
		crit.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit.createAlias("standplaatsRonde.standplaats", "standplaats");
		crit.add(Restrictions.eq("standplaats.id", standplaats.getId()));
		return crit;
	}

	private Criteria createAfsprakenCriteria(boolean bepaalBenodigdeCapaciteit)
	{
		Criteria crit = getSession().createCriteria(MammaAfspraak.class, "afspraak");
		crit.createAlias("afspraak.uitnodiging", "uitnodiging");
		crit.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode");

		if (bepaalBenodigdeCapaciteit)
		{

			crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
			crit.createAlias("screeningRonde.dossier", "dossier");
			crit.createAlias("dossier.client", "client");
			crit.createAlias("client.persoon", "persoon");
		}
		return crit;
	}

}
