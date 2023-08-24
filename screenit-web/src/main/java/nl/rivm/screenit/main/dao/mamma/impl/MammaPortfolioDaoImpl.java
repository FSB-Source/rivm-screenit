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
import java.util.stream.Collectors;

import nl.rivm.screenit.main.dao.mamma.MammaPortfolioDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaPortfolioZoekObject;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaPortfolioDaoImpl extends AbstractAutowiredDao implements MammaPortfolioDao
{
	@Override
	public List<Client> zoekPortfolioClienten(MammaPortfolioZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return getClienten(first, count, sortProperty, ascending, createPortfolioCriteria(zoekObject));
	}

	private List<Client> getClienten(int first, int count, String sortProperty, boolean ascending, Criteria criteria)
	{
		addSortCriteria(criteria, sortProperty, ascending);
		criteria.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			criteria.setMaxResults(count);
		}
		return criteria.list();
	}

	private void addSortCriteria(Criteria criteria, String sortProperty, boolean ascending)
	{
		if (sortProperty != null)
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(sortProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(sortProperty));
			}
		}
	}

	@Override
	public long countPortfolioClienten(MammaPortfolioZoekObject zoekObject)
	{
		Criteria criteria = createPortfolioCriteria(zoekObject);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	@Override
	public List<Long> zoekPortfolioClientenIds(MammaPortfolioZoekObject zoekObject, String sortProperty, boolean ascending)
	{
		Criteria criteria = createPortfolioCriteria(zoekObject);
		criteria.setProjection(Projections.property("id"));
		addSortCriteria(criteria, sortProperty, ascending);
		return criteria.list();
	}

	public Criteria createPortfolioCriteria(MammaPortfolioZoekObject zoekObject)
	{
		Criteria criteria = getSession().createCriteria(Client.class);

		criteria.createAlias("persoon", "persoon");
		criteria.createAlias("mammaDossier", "mammaDossier");
		criteria.add(Subqueries.propertyIn("mammaDossier", createPortfolioSubCriteria(zoekObject)));

		return criteria;
	}

	private DetachedCriteria createPortfolioSubCriteria(MammaPortfolioZoekObject zoekObject)
	{
		DetachedCriteria subCriteria = DetachedCriteria.forClass(MammaMammografie.class);
		subCriteria.createAlias("onderzoek", "onderzoek");
		subCriteria.createAlias("onderzoek.afspraak", "afspraak");
		subCriteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		subCriteria.createAlias("uitnodiging.screeningRonde", "screeningRonde");

		List<InstellingGebruiker> instellingGebruikers = getInstellingGebruikersVoorGebruikers(zoekObject.getGebruikers());

		subCriteria.add(Restrictions.in("afgerondDoor", instellingGebruikers));
		subCriteria.add(Restrictions.eq("ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR));
		subCriteria.add(DateRestrictions.ge("onderzoek.creatieDatum", zoekObject.getVanaf()));
		subCriteria.add(DateRestrictions.le("onderzoek.creatieDatum", zoekObject.getTotEnMet()));

		subCriteria.setProjection(Projections.property("screeningRonde.dossier.id"));

		return subCriteria;
	}

	private List<InstellingGebruiker> getInstellingGebruikersVoorGebruikers(List<Gebruiker> gebruikers)
	{
		return gebruikers.stream().flatMap(gebruiker -> gebruiker.getOrganisatieMedewerkers().stream()).collect(Collectors.toList());
	}

}
