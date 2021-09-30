package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaConclusieReviewDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
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

	private final LocalDate ONDERZOEKEN_MEENEMEN_VANAF = LocalDate.of(2019, 1, 1);

	private final int AANTAL_JAAR_REVIEWS_TERUGZIEN = 1;

	@Override
	public long countScreeningRondesMetConclusie(MammaConclusieReviewZoekObject zoekObject)
	{
		Criteria crit = createScreeningRondeCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaScreeningRonde> zoekScreeningRondesMetConclusie(MammaConclusieReviewZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createScreeningRondeCriteria(zoekObject);

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
		Criteria crit = createScreeningRondeCriteria(zoekObject);
		addSortering(sortProperty, asc, crit);

		crit.createAlias("laatsteOnderzoek.laatsteBeoordeling", "laatsteBeoordeling");
		crit.setProjection(Projections.property("laatsteBeoordeling.id"));

		return crit.list();
	}

	private Criteria createScreeningRondeCriteria(MammaConclusieReviewZoekObject zoekObject)
	{
		InstellingGebruiker instellingGebruiker = zoekObject.getInstellingGebruiker();
		DetachedCriteria subCriteria = DetachedCriteria.forClass(MammaScreeningRonde.class);
		subCriteria.createAlias("uitnodigingen", "uitnodiging");
		subCriteria.createAlias("uitnodiging.afspraken", "afspraak");
		subCriteria.createAlias("afspraak.onderzoek", "onderzoek");
		subCriteria.createAlias("onderzoek.signaleren", "signaleren", JoinType.LEFT_OUTER_JOIN);
		subCriteria.createAlias("onderzoek.beoordelingen", "beoordeling");
		subCriteria.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
		subCriteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);
		subCriteria.createAlias("beoordeling.discrepantieLezing", "discrepantieLezing", JoinType.LEFT_OUTER_JOIN);
		subCriteria.createAlias("beoordeling.arbitrageLezing", "arbitrageLezing", JoinType.LEFT_OUTER_JOIN);
		subCriteria.createAlias("conclusieReviews", "conclusieReviews", JoinType.LEFT_OUTER_JOIN,
			Restrictions.eq("conclusieReviews.radioloog", zoekObject.getInstellingGebruiker()));

		addGereviewedRestriction(zoekObject, subCriteria);

		subCriteria.add(Restrictions.gt("onderzoek.creatieDatum", DateUtil.toUtilDate(ONDERZOEKEN_MEENEMEN_VANAF)));
		subCriteria.add(Restrictions.in("followUpConclusieStatus", MammaFollowUpConclusieStatus.conclusieReviewStatussen()));
		subCriteria.add(Restrictions.or(
			Restrictions.eq("eersteLezing.beoordelaar", instellingGebruiker),
			Restrictions.eq("tweedeLezing.beoordelaar", instellingGebruiker),
			Restrictions.eq("discrepantieLezing.beoordelaar", instellingGebruiker),
			Restrictions.eq("arbitrageLezing.beoordelaar", instellingGebruiker)));

		addFilterOptieRestrictions(zoekObject, instellingGebruiker, subCriteria);
		subCriteria.setProjection(Projections.id());

		Criteria crit = getSession().createCriteria(MammaScreeningRonde.class);
		crit.add(Subqueries.propertyIn("id", subCriteria));

		return crit;
	}

	private void addSortering(String sortProperty, boolean asc, Criteria crit)
	{
		if (sortProperty != null)
		{
			if (sortProperty.startsWith("laatsteOnderzoek."))
			{
				crit.createAlias("laatsteOnderzoek", "laatsteOnderzoek");
			}
			else if (sortProperty.startsWith("persoon."))
			{
				crit.createAlias("dossier", "dossier");
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
			crit.add(Restrictions.ge("conclusieReviews.reviewMoment", currentDateSupplier.getLocalDateTime().minusYears(AANTAL_JAAR_REVIEWS_TERUGZIEN)));
		}
		else
		{
			crit.add(Restrictions.isNull("conclusieReviews.id"));
		}
	}

	private void addFilterOptieRestrictions(MammaConclusieReviewZoekObject zoekObject, InstellingGebruiker instellingGebruiker, DetachedCriteria crit)
	{
		switch (zoekObject.getFilterOptie())
		{
		case FALSE_NEGATIVE_MBB_SIGNALERING:
			crit.add(Restrictions.eq("signaleren.heeftAfwijkingen", true));
			crit.add(Restrictions.eq("followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE));
			break;
		case FALSE_NEGATIVE:
			crit.add(Restrictions.eq("followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE));
			break;
		case FALSE_POSITIVE:
			crit.add(Restrictions.eq("followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_POSITIVE));
			break;
		case TRUE_POSITIVE:
			crit.add(Restrictions.eq("followUpConclusieStatus", MammaFollowUpConclusieStatus.TRUE_POSITIVE));
			break;
		case TRUE_POSITIVE_INDIVIDU_GEMIST:
			crit.add(Restrictions.eq("followUpConclusieStatus", MammaFollowUpConclusieStatus.TRUE_POSITIVE));
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
}
