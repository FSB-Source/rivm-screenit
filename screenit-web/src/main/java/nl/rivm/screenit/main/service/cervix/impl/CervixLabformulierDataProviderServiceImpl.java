package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulier_;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.repository.cervix.CervixLabFormulierRepository;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterBsnCheck;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftDigitaal;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftGeboortedatum;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftLabformulierStatussen;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftMonsterId;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftOrganisatieType;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftScanDatumTotEnMet;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterHeeftScanDatumVanaf;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterLabProcesStapIsControlerenVoorCytologie;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterLabProcesStapIsCytopathologie;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterLabProcesStapIsHuisartsOnbekend;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterLabProcesStapIsHuisartsOnbekendOfControlerenVoorCytologie;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.filterOrganisatieTypeIsScreeningorganisatie;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.heeftGeldigHuisartsbericht;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.persoonJoin;

@Service("cervixLabformulierDataProviderService")
public class CervixLabformulierDataProviderServiceImpl extends RepositoryDataProviderService<CervixLabformulier, CervixLabFormulierRepository, CervixLabformulierenFilter>
{

	@Autowired
	private SessionFactory sessionFactory;

	public static final String CLIENT_PROPERTY =
		CervixLabformulier_.UITSTRIJKJE + "." + CervixMonster_.UITNODIGING + "." + CervixUitnodiging_.SCREENING_RONDE + "." + CervixScreeningRonde_.DOSSIER + "."
			+ CervixDossier_.CLIENT;

	public static final String PERSOON_PROPERTY =
		CLIENT_PROPERTY + "." + Client_.PERSOON;

	@Override
	protected Specification<CervixLabformulier> getSpecification(CervixLabformulierenFilter filter, Sort sortParam)
	{

		return labFormulierSpecification(filter);
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<CervixLabformulier> r, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.startsWith(PERSOON_PROPERTY))
		{
			persoonJoin(r, JoinType.LEFT);
		}
		return null;
	}

	public static Specification<CervixLabformulier> labFormulierSpecification(CervixLabformulierenFilter filter)
	{
		if (filter.getLabprocesStap() == null)
		{
			throw new IllegalStateException("LabprocesStap cannot be null");
		}

		if (filter.getOrganisatieType() != OrganisatieType.RIVM && filter.getOrganisatieType() != OrganisatieType.BMHK_LABORATORIUM
			&& filter.getOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
		{
			throw new IllegalStateException("OrganisatieType isn't compatible");
		}

		return filterHeeftOrganisatieType(filter.getOrganisatieType(), filter.getInstellingId())
			.and(filterHeeftMonsterId(filter.getMonsterId()))
			.and(filterHeeftLabformulierStatussen(filter.getLabformulierStatussen()))
			.and(filterHeeftScanDatumVanaf(filter.getScanDatumVanaf()))
			.and(filterHeeftScanDatumTotEnMet(filter.getScanDatumTotEnMet()))
			.and(filterHeeftGeboortedatum(filter.getGeboortedatum()))
			.and(heeftGeldigHuisartsbericht(filter.getLabprocesStap(), filter.getBsn()))
			.and(filterLabProcesStapIsHuisartsOnbekendOfControlerenVoorCytologie(filter.getLabprocesStap()))
			.and(filterLabProcesStapIsHuisartsOnbekend(filter.getLabprocesStap()))
			.and(filterLabProcesStapIsControlerenVoorCytologie(filter.getLabprocesStap()))
			.and(filterLabProcesStapIsCytopathologie(filter.getLabprocesStap()))
			.and(filterOrganisatieTypeIsScreeningorganisatie(filter.getOrganisatieType(), filter.getInstellingId()))
			.and(filterHeeftDigitaal(filter.getDigitaal()))
			.and(filterBsnCheck(filter.getLabprocesStap(), filter.getBsn()));
	}

	public List<Long> getLabformulierenIds(CervixLabformulierenFilter filter, String sortProperty, boolean asc)
	{
		var currentSession = sessionFactory.getCurrentSession();
		var cb = currentSession.getCriteriaBuilder();
		var q = cb.createQuery(Long.class);
		var r = q.from(CervixLabformulier.class);

		var spec = labFormulierSpecification(filter);
		var predicate = spec.toPredicate(r, q, cb);
		q.where(predicate);

		q.select(r.get(TablePerClassHibernateObject_.id));

		var order = asc ? cb.asc(r.get(sortProperty)) : cb.desc(r.get(sortProperty));
		q.orderBy(order);

		var typedQuery = currentSession.createQuery(q);
		return typedQuery.getResultList();
	}
}
