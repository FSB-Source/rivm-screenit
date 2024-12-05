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

import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht_;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHuisartsBerichtSpecification
{
	public static Specification<CervixHuisartsBericht> heeftGeenHuisartsLocatie()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixHuisartsBericht_.huisartsLocatie));
	}

	public static Specification<CervixHuisartsBericht> heeftHuisartsLocatie()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixHuisartsBericht_.huisartsLocatie));
	}

	public static Specification<CervixHuisartsBericht> heeftStatus(CervixHuisartsBerichtStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixHuisartsBericht_.status), status);
	}

	public static Specification<CervixHuisartsBericht> heeftStatusIn(List<CervixHuisartsBerichtStatus> statuses)
	{
		return (r, q, cb) -> r.get(CervixHuisartsBericht_.status).in(statuses);
	}

	public static Specification<CervixHuisartsBericht> heeftGeenUitstrijkje()
	{
		return (r, q, cb) ->
		{
			var uitstrijkjeJoin = join(r, CervixHuisartsBericht_.uitstrijkje, JoinType.LEFT);
			return cb.isNull(uitstrijkjeJoin.get(SingleTableHibernateObject_.id));
		};
	}
}
