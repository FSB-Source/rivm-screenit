package nl.rivm.screenit.specification.colon;

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

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.HuisartsBericht_;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht_;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonHuisartsberichtSpecification
{
	public static Specification<ColonHuisartsBericht> heeftStatus(ColonHuisartsBerichtStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonHuisartsBericht_.status), status);
	}

	public static Specification<ColonHuisartsBericht> isAangemaaktVanaf(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(HuisartsBericht_.aanmaakDatum), DateUtil.toUtilDate(peilMoment));
	}
}
