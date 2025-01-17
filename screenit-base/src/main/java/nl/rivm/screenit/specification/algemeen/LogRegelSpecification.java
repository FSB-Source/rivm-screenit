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

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LogRegel_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LogRegelSpecification
{
	public static Specification<LogRegel> heeftGebeurtenisDatumVoor(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(LogRegel_.gebeurtenisDatum), toUtilDate(peilDatum));
	}

	public static Specification<LogRegel> heeftBevolkingsonderzoek(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		return (r, q, cb) -> cb.equal(join(r, LogRegel_.bevolkingsonderzoeken), bevolkingsonderzoek);
	}

	public static Specification<LogRegel> heeftAantalBevolkingsonderzoekenGelijkAan(int aantal)
	{
		return (r, q, cb) -> cb.equal(cb.size(r.get(LogRegel_.bevolkingsonderzoeken)), aantal);
	}

	public static Specification<LogRegel> heeftAantalBevolkingsonderzoekenNietGelijkAan(int aantal)
	{
		return (r, q, cb) -> cb.notEqual(cb.size(r.get(LogRegel_.bevolkingsonderzoeken)), aantal);
	}
}
