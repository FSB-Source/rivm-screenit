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

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixCISHistorieSpecification
{
	public static ExtendedSpecification<CervixCISHistorie> heeftGeenScreeningRonde()
	{
		return (r, q, cb) ->
			cb.isNull(r.get(CervixCISHistorie_.screeningRonde));
	}

	public static ExtendedSpecification<CervixCISHistorie> heeftGeenUitstel()
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, CervixCISHistorie_.screeningRonde, JoinType.LEFT);
			return cb.isNull(screeningRondeJoin.get(CervixScreeningRonde_.uitstel));
		};
	}
}
