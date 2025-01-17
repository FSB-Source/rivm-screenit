package nl.rivm.screenit.specification.algemeen;

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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.logging.TechnischeBerichtenLogRegel;
import nl.rivm.screenit.model.logging.TechnischeBerichtenLogRegel_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class TechnischeBerichtenLogRegelSpecification
{
	public static Specification<TechnischeBerichtenLogRegel> heeftRequestMomentVoor(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(TechnischeBerichtenLogRegel_.requestMoment), toUtilDate(peilDatum));
	}
}
