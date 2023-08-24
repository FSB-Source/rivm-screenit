package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.dao.mamma.MammaKwaliteitscontroleDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaAdhocMeekijkverzoekWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieWerklijstZoekObject;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaKwaliteitscontroleDaoImpl extends AbstractAutowiredDao implements MammaKwaliteitscontroleDao
{
	@Override
	public List<MammaFotobespreking> zoekFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createFotobesprekingCriteria(zoekObject);
		if (sortProperty != null)
		{
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();

	}

	private Criteria createFotobesprekingCriteria(MammaFotobesprekingWerklijstZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(MammaFotobespreking.class);
		crit.createAlias("beoordelingsEenheid", "beoordelingsEenheid", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("screeningsEenheid", "screeningsEenheid", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("aangemaaktDoor", "aangemaaktDoor");
		crit.createAlias("aangemaaktDoor.medewerker", "medewerker");
		if (zoekObject.getTotMet() != null)
		{
			crit.add(NvlRestrictions.gt("gestartOp", zoekObject.getTotMet(), Constants.END_OF_TIME));
		}
		if (!zoekObject.getScreeningsEenheden().isEmpty())
		{
			DetachedCriteria subQuery = DetachedCriteria.forClass(MammaScreeningsEenheid.class);
			subQuery.add(Restrictions.in("id", zoekObject.getScreeningsEenheden().stream().map(se -> se.getId()).toArray()));
			subQuery.setProjection(Projections.distinct(Projections.property("beoordelingsEenheid")));
			crit.add(Restrictions.or(Restrictions.in("screeningsEenheid", zoekObject.getScreeningsEenheden()), Subqueries.propertyIn("beoordelingsEenheid", subQuery)));
		}
		if (!zoekObject.getTypen().isEmpty())
		{
			crit.add(Restrictions.in("type", zoekObject.getTypen()));
		}
		return crit;
	}

	@Override
	public long countFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject)
	{
		Criteria crit = createFotobesprekingCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaFotobesprekingOnderzoek> zoekFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject, int first, int count,
		String sortProperty, boolean ascending)
	{
		Criteria crit = createFotobesprekingOnderzoekenCriteria(zoekObject);

		if (sortProperty != null)
		{
			if (sortProperty.startsWith("se."))
			{
				crit.createAlias("onderzoek.screeningsEenheid", "se");
			}
			if (sortProperty.startsWith("persoon."))
			{
				crit.createAlias("onderzoek.afspraak", "afspraak");
				crit.createAlias("afspraak.uitnodiging", "uitnodiging");
				crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
				crit.createAlias("screeningRonde.dossier", "dossier");
				crit.createAlias("dossier.client", "client");
				crit.createAlias("client.persoon", "persoon");
			}
			if (ascending)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createFotobesprekingOnderzoekenCriteria(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(MammaFotobesprekingOnderzoek.class);
		crit.createAlias("beoordeling", "beoordeling");
		crit.createAlias("beoordeling.onderzoek", "onderzoek");
		crit.createAlias("onderzoek.mammografie", "mammografie");
		crit.add(Restrictions.eq("fotobespreking.id", zoekObject.getFotobespreking().getId()));
		return crit;
	}

	@Override
	public long countFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject)
	{
		Criteria crit = createFotobesprekingOnderzoekenCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public boolean isAllesBesproken(MammaFotobespreking fotobespreking)
	{
		Criteria crit = createBesprokenCriteria(fotobespreking);
		crit.add(Restrictions.eq("status", MammaFotobesprekingOnderzoekStatus.NIET_BESPROKEN));
		return (Long) crit.uniqueResult() == 0;
	}

	private Criteria createBesprokenCriteria(MammaFotobespreking fotobespreking)
	{
		Criteria crit = getSession().createCriteria(MammaFotobesprekingOnderzoek.class);
		crit.add(Restrictions.eq("fotobespreking.id", fotobespreking.getId()));
		crit.setProjection(Projections.rowCount());
		return crit;
	}

	@Override
	public Integer getAantalBesproken(MammaFotobespreking fotobespreking)
	{
		Criteria crit = createBesprokenCriteria(fotobespreking);
		crit.add(Restrictions.ne("status", MammaFotobesprekingOnderzoekStatus.NIET_BESPROKEN));
		return ((Long) crit.uniqueResult()).intValue();
	}

	@Override
	public boolean isBeoordelingInBespreking(MammaBeoordeling beoordeling, MammaFotobespreking fotobespreking)
	{
		Criteria crit = getSession().createCriteria(MammaFotobesprekingOnderzoek.class);
		crit.createAlias("fotobespreking", "fotobespreking");
		crit.add(Restrictions.eq("beoordeling", beoordeling));
		crit.add(Restrictions.eq("fotobespreking", fotobespreking));
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult() > 0;

	}

	@Override
	public List<MammaVisitatieOnderzoek> zoekVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject, int first, int count, String sortProperty,
		boolean ascending)
	{
		Criteria crit = createVisitatieOnderzoekenCriteria(zoekObject);

		if (sortProperty != null)
		{
			if (sortProperty.startsWith("se."))
			{
				crit.createAlias("onderzoek.screeningsEenheid", "se");
			}
			if (sortProperty.startsWith("medewerker."))
			{
				crit.createAlias("mammografie.afgerondDoor", "afgerondDoor");
				crit.createAlias("afgerondDoor.medewerker", "medewerker");
			}
			if (sortProperty.startsWith("persoon."))
			{
				crit.createAlias("onderzoek.afspraak", "afspraak");
				crit.createAlias("afspraak.uitnodiging", "uitnodiging");
				crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
				crit.createAlias("screeningRonde.dossier", "dossier");
				crit.createAlias("dossier.client", "client");
				crit.createAlias("client.persoon", "persoon");
			}
			if (ascending)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createVisitatieOnderzoekenCriteria(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(MammaVisitatieOnderzoek.class);
		crit.createAlias("beoordeling", "beoordeling");
		crit.createAlias("beoordeling.onderzoek", "onderzoek");
		crit.createAlias("onderzoek.mammografie", "mammografie");
		crit.add(Restrictions.eq("onderdeel", zoekObject.getOnderdeel()));
		crit.add(Restrictions.eq("visitatie.id", zoekObject.getVisitatie().getId()));
		return crit;
	}

	@Override
	public long countVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject)
	{
		Criteria crit = createVisitatieOnderzoekenCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaVisitatie> zoekVisitaties(MammaVisitatieWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createVisitatieCriteria(zoekObject);
		if (sortProperty != null)
		{
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createVisitatieCriteria(MammaVisitatieWerklijstZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(MammaVisitatie.class);
		crit.createAlias("aangemaaktDoor", "aangemaaktDoor");
		crit.createAlias("aangemaaktDoor.medewerker", "medewerker");
		crit.createAlias("beoordelingsEenheid", "beoordelingsEenheid", JoinType.LEFT_OUTER_JOIN);
		if (zoekObject.getTotMet() != null)
		{
			crit.add(NvlRestrictions.gt("afgerondOp", zoekObject.getTotMet(), Constants.END_OF_TIME));
		}
		if (!zoekObject.getScreeningsEenheden().isEmpty())
		{
			DetachedCriteria subQuery = DetachedCriteria.forClass(MammaScreeningsEenheid.class);
			subQuery.add(Restrictions.in("id", zoekObject.getScreeningsEenheden().stream().map(se -> se.getId()).toArray()));
			subQuery.setProjection(Projections.distinct(Projections.property("beoordelingsEenheid")));
			crit.add(Subqueries.propertyIn("beoordelingsEenheid", subQuery));
		}
		if (!zoekObject.getStatussen().isEmpty())
		{
			crit.add(Restrictions.in("status", zoekObject.getStatussen()));
		}
		return crit;
	}

	@Override
	public long countVisitaties(MammaVisitatieWerklijstZoekObject zoekObject)
	{
		Criteria crit = createVisitatieCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public Integer getAantalGezien(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel)
	{
		Criteria crit = createGezienCriteria(visitatie, onderdeel);
		crit.add(Restrictions.ne("status", MammaVisitatieOnderzoekStatus.NIET_GEZIEN));
		return ((Long) crit.uniqueResult()).intValue();
	}

	private Criteria createGezienCriteria(MammaVisitatie vistiatie, MammaVisitatieOnderdeel onderdeel)
	{
		Criteria crit = getSession().createCriteria(MammaVisitatieOnderzoek.class);
		crit.add(Restrictions.eq("visitatie.id", vistiatie.getId()));
		if (onderdeel != null)
		{
			crit.add(Restrictions.eq("onderdeel", onderdeel));
		}
		crit.setProjection(Projections.rowCount());
		return crit;
	}

	@Override
	public boolean isAllesGezien(MammaVisitatie visitatie)
	{
		Criteria crit = createGezienCriteria(visitatie, null);
		crit.add(Restrictions.eq("status", MammaVisitatieOnderzoekStatus.NIET_GEZIEN));
		return (Long) crit.uniqueResult() == 0;
	}

	@Override
	public boolean isBeoordelingInVisitatieOnderdeel(MammaBeoordeling beoordeling, MammaVisitatie visitatie, MammaVisitatieOnderdeel visitatieOnderdeel)
	{
		Criteria crit = getSession().createCriteria(MammaVisitatieOnderzoek.class);
		crit.createAlias("visitatie", "visitatie");
		crit.add(Restrictions.eq("beoordeling", beoordeling));
		crit.add(Restrictions.eq("visitatie", visitatie));
		crit.add(Restrictions.eq("onderdeel", visitatieOnderdeel));
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult() > 0;
	}

	@Override
	public boolean nieuweBeoordelingenAangevraagdNavFotobespreking(MammaFotobespreking fotobespreking)
	{
		Criteria crit = createBesprokenCriteria(fotobespreking);
		crit.add(Restrictions.eq("status", MammaFotobesprekingOnderzoekStatus.NIEUWE_BEOORDELING_AANGEMAAKT));
		return (Long) crit.uniqueResult() > 0;
	}

	@Override
	public List<MammaAdhocMeekijkverzoek> zoekAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject, int first, int count,
		SortState<String> sortState)
	{
		Criteria crit = createAdhocMeekijkverzoekOnderzoekenCriteria(zoekObject);
		String sortProperty = sortState.getSortParam();
		if (sortProperty != null)
		{
			if (sortProperty.startsWith("se."))
			{
				crit.createAlias("onderzoek.screeningsEenheid", "se");
			}
			if (sortProperty.startsWith("medewerker."))
			{
				crit.createAlias("onderzoek.mammografie", "mammografie");
				crit.createAlias("mammografie.afgerondDoor", "afgerondDoor");
				crit.createAlias("afgerondDoor.medewerker", "medewerker");
			}
			if (sortProperty.startsWith("persoon."))
			{
				crit.createAlias("onderzoek.afspraak", "afspraak");
				crit.createAlias("afspraak.uitnodiging", "uitnodiging");
				crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
				crit.createAlias("screeningRonde.dossier", "dossier");
				crit.createAlias("dossier.client", "client");
				crit.createAlias("client.persoon", "persoon");
			}
			if (sortState.isAsc())
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createAdhocMeekijkverzoekOnderzoekenCriteria(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(MammaAdhocMeekijkverzoek.class);
		crit.createAlias("onderzoek", "onderzoek");
		if (zoekObject.getScreeningsEenheden().size() > 0)
		{
			crit.add(Restrictions.in("onderzoek.screeningsEenheid", zoekObject.getScreeningsEenheden()));
		}
		if (zoekObject.getStatus() != null)
		{
			crit.add(Restrictions.eq("status", zoekObject.getStatus()));
		}
		return crit;
	}

	@Override
	public long countAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject)
	{
		Criteria crit = createAdhocMeekijkverzoekOnderzoekenCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public Integer getAantalGezienAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject)
	{
		Criteria crit = createAdhocMeekijkverzoekOnderzoekenCriteria(zoekObject);
		crit.add(Restrictions.ne("status", MammaVisitatieOnderzoekStatus.NIET_GEZIEN));
		crit.setProjection(Projections.rowCount());
		return ((Long) crit.uniqueResult()).intValue();
	}

	@Override
	public Integer getAantalGezienAdhocMeekijkverzoekOnderzoekenInList(List<Long> onderzoekenIds)
	{
		Criteria crit = getSession().createCriteria(MammaAdhocMeekijkverzoek.class);
		crit.add(Restrictions.in("id", onderzoekenIds));
		crit.add(Restrictions.ne("status", MammaVisitatieOnderzoekStatus.NIET_GEZIEN));
		crit.setProjection(Projections.rowCount());
		return ((Long) crit.uniqueResult()).intValue();
	}
}
