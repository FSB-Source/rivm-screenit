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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer_;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonIntakelocatieSpecification
{
	public static Specification<ColonIntakelocatie> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(Instelling_.actief));
	}

	public static Specification<ColonIntakelocatie> heeftGeenCapaciteitBinnenDatum(Range<LocalDate> bereik)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonIntakekamer.class);
			var subqueryRoot = subquery.from(ColonIntakekamer.class);
			var tijdslotsJoin = SpecificationUtil.join(subqueryRoot, ColonIntakekamer_.tijdslots);

			subquery.select(subqueryRoot).where(
				cb.and(
					cb.equal(subqueryRoot.get(ColonIntakekamer_.intakelocatie), r),
					heeftVanafIn(bereik).toPredicate(tijdslotsJoin, q, cb),
					cb.equal(tijdslotsJoin.get(ColonTijdslot_.type), ColonTijdslotType.AFSPRAAKSLOT))
			);

			return cb.not(cb.exists(subquery));
		};
	}

	private static ExtendedSpecification<ColonTijdslot> heeftVanafIn(Range<LocalDate> bereik)
	{
		return bevatLocalDate(bereik, c -> c.get(ColonTijdslot_.vanaf));
	}

	public static Specification<ColonIntakelocatie> isIntakelocatie(ColonIntakelocatie intakelocatie)
	{
		return (r, q, cb) -> cb.equal(r, intakelocatie);
	}
}
