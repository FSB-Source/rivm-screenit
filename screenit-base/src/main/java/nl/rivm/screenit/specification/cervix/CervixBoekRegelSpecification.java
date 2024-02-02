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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

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

	public static Specification<CervixBoekRegel> metOpdrachtID(Long id)
	{
		return (r, q, cb) ->
		{
			var betaalopdrachtPath = r
				.get(CervixBoekRegel_.specificatie)
				.get(CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel)
				.get(CervixBetaalopdrachtRegel_.betaalopdracht);

			return cb.equal(betaalopdrachtPath.get(AbstractHibernateObject_.id), id);
		};
	}

	public static Specification<CervixBoekRegel> isHuisartsBetaalopdrachtRegel()
	{
		return (r, q, cb) ->
		{
			var betaalopdrachtRegelPath = r
				.get(CervixBoekRegel_.specificatie)
				.get(CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel);

			return cb.isNotNull(betaalopdrachtRegelPath.get(CervixBetaalopdrachtRegel_.huisartsLocatie));
		};

	}

	public static Specification<CervixBoekRegel> metDebet(boolean debet)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixBoekRegel_.debet), debet);
	}

}
