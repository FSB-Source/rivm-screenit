package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.repository.cervix.CervixBoekRegelRepository;
import nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterBsn;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterGeboortedatum;
import static nl.rivm.screenit.specification.cervix.CervixBetaalopdrachtSpecification.filterOpBetalingskenmerkContaining;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.filterAlleenVerrichtingen;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.filterAlleenZonderBetalingskenmerk;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.filterDebet;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.huisartsLocatieJoin;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.labformulierJoin;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.monsterJoin;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.persoonJoin;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.verrichtingJoin;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.filterMonsterId;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterScreeningOrganisatie;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterVerrichtingType;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.verrichtingsdatumValtTussenVoorBoekRegel;
import static org.springframework.data.domain.Sort.Direction.ASC;

public abstract class AbstractCervixBoekregelsDataProviderServiceImpl
	extends RepositoryDataProviderService<CervixBoekRegel, CervixBoekRegelRepository, CervixVerrichtingenZoekObject>
{
	public static final String PERSOON_PROPERTY = CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.CLIENT + "." + Client_.PERSOON;

	public static final String REGIO_PROPERTY = CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.REGIO;

	public static final String MONSTER_PROPERTY = CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.MONSTER;

	public static final String BETAALOPDRACHT_PROPERTY =
		CervixBoekRegel_.SPECIFICATIE + "." + CervixBetaalopdrachtRegelSpecificatie_.BETAALOPDRACHT_REGEL + "." + CervixBetaalopdrachtRegel_.BETAALOPDRACHT;

	public static final String LABFORMULIER_PROPERTY = MONSTER_PROPERTY + "." + CervixUitstrijkje_.LABFORMULIER;

	public static final String HUISARTS_LOCATIE_PROPERTY = CervixBoekRegel_.VERRICHTING + "." + CervixVerrichting_.HUISARTS_LOCATIE;

	@Override
	protected Specification<CervixBoekRegel> getSpecification(CervixVerrichtingenZoekObject filter, Sort sortParam)
	{
		return filterAlleenVerrichtingen(filter.isAlleenVerrichtingen())
			.and(filterAlleenZonderBetalingskenmerk(filter.isAlleenZonderBetalingskenmerk()))
			.and(filterBsn(filter.getBsn()).withRoot(CervixBoekRegelSpecification::persoonJoin))
			.and(filterDebet(filter.getDebet()))
			.and(filterOpBetalingskenmerkContaining(filter.getBetalingskenmerk()))
			.and(filterGeboortedatum(filter.getGeboorteDatum()).withRoot(CervixBoekRegelSpecification::persoonJoin))
			.and(filterMonsterId(filter.getMonsterId()))
			.and(filterScreeningOrganisatie(filter.getScreeningOrganisatie()))
			.and(verrichtingsdatumValtTussenVoorBoekRegel(DateUtil.toLocalDate(filter.getVerrichtingsDatumVanaf()), DateUtil.toLocalDate(filter.getVerrichtingsDatumTotenmet())))
			.and(filterVerrichtingType(filter.getVerrichtingsType()));
	}

	@Override
	protected Sort getSort(Sort sort)
	{
		return sort
			.and(Sort.by(ASC, MONSTER_PROPERTY + "." + CervixMonster_.MONSTER_ID))
			.and(Sort.by(ASC, AbstractHibernateObject_.ID));
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<CervixBoekRegel> r, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.startsWith(LABFORMULIER_PROPERTY))
		{
			var property = sortProperty.substring(LABFORMULIER_PROPERTY.length() + 1);
			var exp = labformulierJoin(r, cb, JoinType.LEFT).get(property);
			return order.isAscending() ? cb.asc(exp) : cb.desc(exp);
		}
		else if (sortProperty.startsWith(HUISARTS_LOCATIE_PROPERTY))
		{
			huisartsLocatieJoin(r);
		}
		else if (sortProperty.startsWith(MONSTER_PROPERTY))
		{
			monsterJoin(r);
		}
		else if (sortProperty.startsWith(PERSOON_PROPERTY))
		{
			persoonJoin(r);
		}
		else if (sortProperty.startsWith(REGIO_PROPERTY))
		{
			join(verrichtingJoin(r), CervixVerrichting_.regio);
		}
		else if (sortProperty.startsWith(BETAALOPDRACHT_PROPERTY))
		{
			join(join(join(r, CervixBoekRegel_.specificatie, JoinType.LEFT), CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel, JoinType.LEFT),
				CervixBetaalopdrachtRegel_.betaalopdracht, JoinType.LEFT);
		}
		return null;
	}

	@Override
	protected Type[] getActualTypeArguments()
	{
		return ((ParameterizedType) ((Class<?>) getClass().getGenericSuperclass()).getGenericSuperclass()).getActualTypeArguments();
	}
}
