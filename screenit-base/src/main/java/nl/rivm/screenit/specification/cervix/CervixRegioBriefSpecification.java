package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioBrief_;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixRegioBriefSpecification
{
	public static Specification<CervixRegioBrief> heeftGeenMergedBrieven()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixRegioBrief_.mergedBrieven));
	}

	public static ExtendedSpecification<CervixRegioBrief> heeftMergedBrieven(CervixRegioMergedBrieven mergedBrieven)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixRegioBrief_.mergedBrieven), mergedBrieven);
	}

	public static ExtendedSpecification<CervixRegioBrief> heeftHuisarts(CervixHuisarts huisarts)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixRegioBrief_.huisarts), huisarts);
	}

	public static ExtendedSpecification<CervixRegioBrief> heeftScreeningOrganisatieId(Long soId)
	{
		return (r, q, cb) ->
		{
			var organisatieJoin = join(r, CervixRegioBrief_.regio);
			return cb.equal(organisatieJoin.get(SingleTableHibernateObject_.id), soId);
		};
	}
}
