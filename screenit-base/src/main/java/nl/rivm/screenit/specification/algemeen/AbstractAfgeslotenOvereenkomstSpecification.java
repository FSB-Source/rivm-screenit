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

import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst_;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.DateSpecification.extractYear;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AbstractAfgeslotenOvereenkomstSpecification
{
	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> filterActief(Boolean actief, Date peilmoment)
	{
		return (r, q, cb) ->
		{
			if (actief == null)
			{
				return null;
			}

			var vanafExpression = cb.coalesce(r.get(AbstractAfgeslotenOvereenkomst_.startDatum), DateUtil.BEGIN_OF_TIME);
			var totEnMetExpression = cb.coalesce(r.get(AbstractAfgeslotenOvereenkomst_.eindDatum), DateUtil.END_OF_TIME);
			var predicate = bevat(ri -> vanafExpression, ri -> totEnMetExpression, Pair.of(BoundType.CLOSED, BoundType.CLOSED), peilmoment).toPredicate(r, q, cb);
			if (Boolean.TRUE.equals(actief))
			{
				return predicate;
			}
			return cb.not(predicate);
		};
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> heeftOvereenkomst(Overeenkomst overeenkomst)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractAfgeslotenOvereenkomst_.overeenkomst), overeenkomst);
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> bevatPeildatum(Date peilmoment)
	{
		return (r, q, cb) ->
		{
			var vanafExpression = r.get(AbstractAfgeslotenOvereenkomst_.startDatum);
			var totEnMetExpression = cb.coalesce(r.get(AbstractAfgeslotenOvereenkomst_.eindDatum), DateUtil.END_OF_TIME);
			return bevat(ri -> vanafExpression, ri -> totEnMetExpression, Pair.of(BoundType.CLOSED, BoundType.CLOSED), peilmoment).toPredicate(r, q, cb);
		};
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> heeftStartDatumInJaar(int jaar)
	{
		return (r, q, cb) -> cb.equal(extractYear(r.get(AbstractAfgeslotenOvereenkomst_.startDatum), cb), jaar);
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> isNietGeaccodeerd()
	{
		return (r, q, cb) -> cb.and(cb.isNull(r.get(AbstractAfgeslotenOvereenkomst_.akkoordDatum)), cb.isTrue(r.get(AbstractAfgeslotenOvereenkomst_.teAccoderen)));
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> heeftEinddatumVanaf(Date peildatum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(cb.coalesce(r.get(AbstractAfgeslotenOvereenkomst_.eindDatum), DateUtil.END_OF_TIME), peildatum);
	}
}
