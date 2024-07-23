package nl.rivm.screenit.specification.cervix;

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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNotTrue;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixBoekRegelSpecification
{
	public static Specification<CervixBoekRegel> baseSpecification()
	{
		return (r, q, cb) -> cb.isNotNull(r);
	}

	public static Specification<CervixBoekRegel> metSpecificatie()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixBoekRegel_.specificatie));
	}

	public static PathAwarePredicate<CervixBoekRegel> heeftNogGeenBetaalopdracht()
	{
		return (cb, r) -> cb.isNull(r.get(CervixBoekRegel_.specificatie));
	}

	public static Specification<CervixBoekRegel> metDebet(boolean debet)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixBoekRegel_.debet), debet);
	}

	public static Specification<CervixBoekRegel> filterDebet(Boolean debet)
	{
		return skipWhenNull(debet, (r, q, cb) -> cb.equal(r.get(CervixBoekRegel_.debet), debet));
	}

	public static Specification<CervixBoekRegel> filterAlleenVerrichtingen(Boolean alleenVerrichtingen)
	{
		return skipWhenNotTrue(alleenVerrichtingen, (r, q, cb) -> cb.equal(r.get(AbstractHibernateObject_.id),
			join(verrichtingJoin(r), CervixVerrichting_.laatsteBoekRegel).get(AbstractHibernateObject_.id)));
	}

	public static Specification<CervixBoekRegel> filterAlleenZonderBetalingskenmerk(Boolean alleenZonderBetalingskenmerk)
	{
		return skipWhenNotTrue(alleenZonderBetalingskenmerk, (r, q, cb) -> heeftNogGeenBetaalopdracht().withPath(cb, r));
	}

	public static Join<CervixBoekRegel, CervixVerrichting> verrichtingJoin(Root<CervixBoekRegel> r)
	{
		return join(r, CervixBoekRegel_.verrichting);
	}

	public static Join<CervixVerrichting, CervixMonster> monsterJoin(Root<CervixBoekRegel> r)
	{
		return join(verrichtingJoin(r), CervixVerrichting_.monster);
	}

	private static Join<CervixVerrichting, CervixUitstrijkje> monsterToUitstrijkje(Root<CervixBoekRegel> r, CriteriaBuilder cb)
	{
		return cb.treat(monsterJoin(r), CervixUitstrijkje.class);
	}

	public static Join<CervixUitstrijkje, CervixLabformulier> labformulierJoin(Root<CervixBoekRegel> r, CriteriaBuilder cb, JoinType left)
	{
		return join(monsterToUitstrijkje(r, cb), CervixUitstrijkje_.labformulier, left);
	}

	public static From<CervixVerrichting, CervixHuisartsLocatie> huisartsLocatieJoin(Root<CervixBoekRegel> r)
	{
		return join(verrichtingJoin(r), CervixVerrichting_.huisartsLocatie);
	}

	static From<CervixBetaalopdrachtRegel, CervixBetaalopdracht> betaalopdrachtJoin(Root<CervixBoekRegel> r)
	{
		return join(join(join(r, CervixBoekRegel_.specificatie), CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel), CervixBetaalopdrachtRegel_.betaalopdracht);
	}

	public static From<Client, GbaPersoon> persoonJoin(Root<CervixBoekRegel> r)
	{
		return join(clientJoin(r), Client_.persoon);
	}

	private static From<CervixVerrichting, Client> clientJoin(Root<CervixBoekRegel> r)
	{
		return join(verrichtingJoin(r), CervixVerrichting_.client);
	}

}
