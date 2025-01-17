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

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.InstellingGebruikerRol_;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieMedewerkerRolSpecification
{
	public static ExtendedSpecification<InstellingGebruikerRol> isActiefOpDatum(LocalDateTime peildatum)
	{
		return (r, q, cb) ->
		{
			var beginExpression = cb.coalesce(r.get(InstellingGebruikerRol_.beginDatum), DateUtil.BEGIN_OF_TIME);
			var eindExpression = cb.coalesce(r.get(InstellingGebruikerRol_.eindDatum), DateUtil.END_OF_TIME);
			return cb.and(cb.isTrue(r.get(InstellingGebruikerRol_.actief)),
				bevat(ri -> beginExpression, ri -> eindExpression, Pair.of(
					BoundType.CLOSED, BoundType.CLOSED), DateUtil.toUtilDate(peildatum)).toPredicate(r, q, cb));
		};
	}

	public static ExtendedSpecification<InstellingGebruikerRol> isActief(Boolean waarde)
	{
		return (r, q, cb) -> cb.equal(r.get(InstellingGebruikerRol_.actief), waarde);
	}

	public static ExtendedSpecification<InstellingGebruikerRol> heeftRol(Rol rol)
	{
		return (r, q, cb) -> cb.equal(r.get(InstellingGebruikerRol_.rol), rol);
	}
}
