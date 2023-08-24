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

import java.time.LocalDate;

import nl.rivm.screenit.main.dao.mamma.MammaLezingRapportageDao;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.rivm.screenit.util.query.DateYearRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaLezingRapportageDaoImpl extends AbstractAutowiredDao implements MammaLezingRapportageDao
{

	@Override
	public long eersteLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return countLezingenVoorRol(instellingGebruiker, date, MammaBeoordelingStatus.EERSTE_LEZING, termijn);
	}

	@Override
	public long tweedeLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return countLezingenVoorRol(instellingGebruiker, date, MammaBeoordelingStatus.TWEEDE_LEZING, termijn);
	}

	@Override
	public long eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		Criteria criteria = createBaseCriteria(instellingGebruiker, date, termijn);
		criteria.add(Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG));
		DetachedCriteria subQuery = createSubQueryScreeningRondeCount();
		criteria.add(Subqueries.lt(1L, subQuery));
		return (Long) criteria.setProjection(Projections.rowCount()).uniqueResult();
	}

	private DetachedCriteria createSubQueryScreeningRondeCount()
	{
		DetachedCriteria subQuery = DetachedCriteria.forClass(MammaScreeningRonde.class, "ro");
		subQuery.add(Restrictions.eqProperty("ro.dossier", "dos.id"));
		subQuery.setProjection(Projections.rowCount());
		return subQuery;
	}

	@Override
	public long discrepantieLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate localDate, Termijn termijn)
	{
		Criteria criteria = createBaseCriteria(instellingGebruiker, localDate, termijn);
		criteria.add(Restrictions.or(
			Restrictions.isNotNull("beoordeling.discrepantieLezing"),
			Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.DISCREPANTIE)));
		return (Long) criteria.setProjection(Projections.rowCount()).uniqueResult();
	}

	@Override
	public long eersteOf2deLezerOngunstigeUitslagEersteRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		Criteria criteria = createBaseCriteria(instellingGebruiker, date, termijn);
		criteria.add(Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG));
		DetachedCriteria subQuery = createSubQueryScreeningRondeCount();
		criteria.add(Subqueries.eq(1L, subQuery));
		return (Long) criteria.setProjection(Projections.rowCount()).uniqueResult();
	}

	private Criteria createBaseCriteria(InstellingGebruiker instellingGebruiker, LocalDate localDate, Termijn termijn)
	{
		DetachedCriteria lezingCrit = DetachedCriteria.forClass(MammaLezing.class);
		lezingCrit.add(Restrictions.eq("beoordelaar", instellingGebruiker));
		switch (termijn)
		{
			case VANDAAG:
				lezingCrit.add(DateRestrictions.eq("beoordelingDatum", DateUtil.toUtilDate(localDate)));
				break;
			case KALENDERJAAR:
				lezingCrit.add(DateYearRestrictions.eq("beoordelingDatum", localDate.getYear()));
				break;
		}
		lezingCrit.setProjection(Projections.id());

		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class, "beoordeling");
		criteria.createAlias("onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");
		criteria.createAlias("ronde.dossier", "dos");
		criteria.add(
			Restrictions.or(
				Subqueries.propertyIn("eersteLezing.id", lezingCrit),
				Subqueries.propertyIn("tweedeLezing.id", lezingCrit) 
			) 
		);
		return criteria;
	}

	private long countLezingenVoorRol(InstellingGebruiker gebruiker, LocalDate localDate, MammaBeoordelingStatus beoordelingStatus, Termijn termijn)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class);
		String associationPath;
		switch (beoordelingStatus)
		{
		case EERSTE_LEZING:
			associationPath = "eersteLezing";
			break;
		case TWEEDE_LEZING:
			associationPath = "tweedeLezing";
			break;
		case DISCREPANTIE:
		case ARBITRAGE:
		case VERSLAG_MAKEN:
		default:
			throw new IllegalStateException(beoordelingStatus.getNaam() + " is geen geldige status");
		}
		criteria.createAlias(associationPath, "lezing");
		criteria.add(Restrictions.eq("lezing.beoordelaar", gebruiker));
		switch (termijn)
		{
		case VANDAAG:
			criteria.add(DateRestrictions.eq("lezing.beoordelingDatum", DateUtil.toUtilDate(localDate)));
			break;
		case KALENDERJAAR:
			criteria.add(DateYearRestrictions.eq("lezing.beoordelingDatum", localDate.getYear()));
			break;
		}
		return (Long) criteria.setProjection(Projections.rowCount()).uniqueResult();
	}

}
