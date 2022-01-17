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

import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaConclusieReviewDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.SQLQuery;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaConclusieReviewDaoImpl extends AbstractAutowiredDao implements MammaConclusieReviewDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private final int AANTAL_JAAR_REVIEWS_TERUGZIEN = 1;

	@Override
	public long countConclusieReviewsVanRadioloog(MammaConclusieReviewZoekObject zoekObject)
	{
		Criteria crit = createConclusieReviewCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaConclusieReview> zoekConclusieReviewsVanRadioloog(MammaConclusieReviewZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createConclusieReviewCriteria(zoekObject);

		addSortering(sortProperty, asc, crit);

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}

		return crit.list();
	}

	@Override
	public List<Long> zoekBeoordelingIdsMetConclusie(MammaConclusieReviewZoekObject zoekObject, String sortProperty, boolean asc)
	{
		Criteria crit = createConclusieReviewCriteria(zoekObject);
		addSortering(sortProperty, asc, crit);

		crit.createAlias("laatsteOnderzoek.laatsteBeoordeling", "laatsteBeoordeling");
		crit.setProjection(Projections.property("laatsteBeoordeling.id"));

		return crit.list();
	}

	private Criteria createConclusieReviewCriteria(MammaConclusieReviewZoekObject zoekObject)
	{
		InstellingGebruiker instellingGebruiker = zoekObject.getInstellingGebruiker();
		DetachedCriteria subCriteria = DetachedCriteria.forClass(MammaConclusieReview.class);
		subCriteria.createAlias("screeningRonde", "screeningRonde");

		addGereviewedRestriction(zoekObject, subCriteria);

		subCriteria.add(Restrictions.eq("radioloog", zoekObject.getInstellingGebruiker()));

		addFilterOptieRestrictions(zoekObject, instellingGebruiker, subCriteria);
		subCriteria.setProjection(Projections.id());

		Criteria crit = getSession().createCriteria(MammaConclusieReview.class);
		crit.add(Subqueries.propertyIn("id", subCriteria));

		return crit;
	}

	private void addSortering(String sortProperty, boolean asc, Criteria crit)
	{
		if (sortProperty != null)
		{
			crit.createAlias("screeningRonde", "screeningRonde");
			if (sortProperty.startsWith("laatsteOnderzoek."))
			{
				crit.createAlias("screeningRonde.laatsteOnderzoek", "laatsteOnderzoek");
			}
			else if (sortProperty.startsWith("persoon."))
			{
				crit.createAlias("screeningRonde.dossier", "dossier");
				crit.createAlias("dossier.client", "client");
				crit.createAlias("client.persoon", "persoon");
			}
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
	}

	private void addGereviewedRestriction(MammaConclusieReviewZoekObject zoekObject, DetachedCriteria crit)
	{
		if (zoekObject.getGezienTonen())
		{
			LocalDateTime beginKalenderJaar = currentDateSupplier.getLocalDate().with(TemporalAdjusters.firstDayOfYear()).atStartOfDay();
			LocalDateTime tonenNaDatum = zoekObject.getVoorDashboard() ? beginKalenderJaar : currentDateSupplier.getLocalDateTime().minusYears(AANTAL_JAAR_REVIEWS_TERUGZIEN);

			crit.add(Restrictions.ge("reviewMoment", tonenNaDatum));
		}
		else
		{
			crit.add(Restrictions.isNull("reviewMoment"));
		}
	}

	private void addFilterOptieRestrictions(MammaConclusieReviewZoekObject zoekObject, InstellingGebruiker instellingGebruiker, DetachedCriteria crit)
	{
		switch (zoekObject.getFilterOptie())
		{
		case FALSE_NEGATIVE_MBB_SIGNALERING:
			crit.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
			crit.createAlias("uitnodiging.afspraken", "afspraak");
			crit.createAlias("afspraak.onderzoek", "onderzoek");
			crit.createAlias("onderzoek.signaleren", "signaleren");

			crit.add(Restrictions.eq("signaleren.heeftAfwijkingen", true));
			crit.add(Restrictions.eq("screeningRonde.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE));
			break;
		case FALSE_NEGATIVE:
			crit.add(Restrictions.eq("screeningRonde.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE));
			break;
		case FALSE_POSITIVE:
			crit.add(Restrictions.eq("screeningRonde.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_POSITIVE));
			break;
		case TRUE_POSITIVE:
			crit.add(Restrictions.eq("screeningRonde.followUpConclusieStatus", MammaFollowUpConclusieStatus.TRUE_POSITIVE));
			break;
		case TRUE_POSITIVE_INDIVIDU_GEMIST:
			crit.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
			crit.createAlias("uitnodiging.afspraken", "afspraak");
			crit.createAlias("afspraak.onderzoek", "onderzoek");
			crit.createAlias("onderzoek.beoordelingen", "beoordeling");
			crit.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("beoordeling.discrepantieLezing", "discrepantieLezing", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("beoordeling.arbitrageLezing", "arbitrageLezing", JoinType.LEFT_OUTER_JOIN);

			crit.add(Restrictions.eq("screeningRonde.followUpConclusieStatus", MammaFollowUpConclusieStatus.TRUE_POSITIVE));
			crit.add(Restrictions.or(
				heeftNietVerwezenLezing("eersteLezing", instellingGebruiker),
				heeftNietVerwezenLezing("tweedeLezing", instellingGebruiker),
				heeftNietVerwezenLezing("arbitrageLezing", instellingGebruiker)));
			break;
		}
	}

	private Conjunction heeftNietVerwezenLezing(String lezing, InstellingGebruiker instellingGebruiker)
	{
		return Restrictions.and(
			Restrictions.in(lezing + ".biradsLinks", MammaBIRADSWaarde.getNietVerwijzendBIRADSWaarden()),
			Restrictions.in(lezing + ".biradsRechts", MammaBIRADSWaarde.getNietVerwijzendBIRADSWaarden()),
			Restrictions.eq(lezing + ".beoordelaar", instellingGebruiker));
	}

	@Override
	public MammaConclusieReview getConclusieReview(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog)
	{
		Criteria criteria = getSession().createCriteria(MammaConclusieReview.class, "conclusieReview");
		criteria.add(Restrictions.eq("conclusieReview.screeningRonde.id", screeningRonde.getId()));
		criteria.add(Restrictions.eq("conclusieReview.radioloog.id", radioloog.getId()));

		return (MammaConclusieReview) criteria.uniqueResult();
	}

	@Override
	public List<InstellingGebruiker> getRadiologenMetLezingVanRondeEnZonderReview(MammaScreeningRonde screeningRonde)
	{
		StringBuilder queryString = new StringBuilder();
		queryString.append("select distinct {ig.*}");

		queryString.append(" from algemeen.instelling_gebruiker ig");

		queryString.append(" inner join algemeen.org_organisatie_medewerker ig_1_ on ig.id = ig_1_.id");
		queryString.append(" inner join mamma.lezing l on ig.id = l.beoordelaar");
		queryString.append(" inner join mamma.beoordeling b on l.id = b.eerste_lezing or l.id = b.tweede_lezing or l.id = b.arbitrage_lezing or l.id = b.discrepantie_lezing");
		queryString.append(" inner join mamma.onderzoek o on b.onderzoek = o.id");
		queryString.append(" inner join mamma.afspraak a on o.id = a.onderzoek");
		queryString.append(" inner join mamma.uitnodiging u on a.uitnodiging = u.id");
		queryString.append(" inner join mamma.screening_ronde sr on u.screening_ronde = sr.id");
		queryString.append(" left outer join mamma.conclusie_review cr on sr.id = cr.screening_ronde and cr.radioloog = ig.id");
		queryString.append(" where sr.id = :screeningRondeId");
		queryString.append(" and cr.id is null");

		SQLQuery query = getSession().createSQLQuery(queryString.toString()).addEntity("ig", InstellingGebruiker.class);
		query.setParameter("screeningRondeId", screeningRonde.getId());

		return (List<InstellingGebruiker>) query.list();
	}
}
