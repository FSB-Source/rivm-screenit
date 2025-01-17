package nl.rivm.screenit.specification.colon;

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
import java.time.LocalDateTime;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.overlaptLocalDate;
import static nl.rivm.screenit.specification.RangeSpecification.overlapt;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonTijdslotSpecification
{
	public static <T extends ColonTijdslot> ExtendedSpecification<T> heeftKamer(ColonIntakekamer kamer)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.kamer), kamer);
	}

	public static <T extends ColonTijdslot> ExtendedSpecification<T> heeftKamerUitLijst(List<ColonIntakekamer> kamers)
	{
		return (r, q, cb) -> r.get(ColonTijdslot_.kamer).in(kamers);
	}

	public static <T extends ColonTijdslot> ExtendedSpecification<T> heeftVanaf(LocalDateTime vanaf)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.vanaf), vanaf);
	}

	public static <T extends ColonTijdslot> ExtendedSpecification<T> valtBinnenDatumTijdRange(Range<LocalDateTime> range)
	{
		return overlapt(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot));
	}

	public static <T extends ColonTijdslot> ExtendedSpecification<T> valtBinnenDatumRange(Range<LocalDate> range)
	{
		return overlaptLocalDate(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot));
	}

	public static <T extends ColonTijdslot> ExtendedSpecification<T> isAfspraakslot()
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.type), ColonTijdslotType.AFSPRAAKSLOT);
	}
}
