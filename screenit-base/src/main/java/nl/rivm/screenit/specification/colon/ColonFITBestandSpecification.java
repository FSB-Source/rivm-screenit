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

import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTBestand_;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFITBestandSpecification
{
	public static Specification<IFOBTBestand> heeftStatussen(List<IFOBTBestandStatus> statussen)
	{
		return (r, q, cb) -> r.get(IFOBTBestand_.status).in(statussen);
	}

	public static Specification<IFOBTBestand> heeftBestandsnaam(String bestandsnaam)
	{
		return (r, q, cb) -> cb.equal(r.get(IFOBTBestand_.naamBestand), bestandsnaam);
	}

	public static ExtendedSpecification<IFOBTBestand> heeftStatus(IFOBTBestandStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(IFOBTBestand_.status), status);
	}

	public static ExtendedSpecification<IFOBTBestand> filterStatus(IFOBTBestandStatus status)
	{
		return (r, q, cb) ->
		{
			List<IFOBTBestandStatus> statussen = List.of();
			if (status == null)
			{
				statussen = List.of(IFOBTBestandStatus.GEAUTORISEERD, IFOBTBestandStatus.INGELEZEN, IFOBTBestandStatus.VERWERKT);
			}
			else if (status == IFOBTBestandStatus.VERWERKT)
			{
				statussen = List.of(IFOBTBestandStatus.GEAUTORISEERD, IFOBTBestandStatus.VERWERKT);
			}
			else if (status == IFOBTBestandStatus.INGELEZEN)
			{
				statussen = List.of(IFOBTBestandStatus.INGELEZEN);
			}
			return r.get(IFOBTBestand_.status).in(statussen);
		};
	}

	public static ExtendedSpecification<IFOBTBestand> filterLaboratorium(IFobtLaboratorium laboratorium)
	{
		return skipWhenNullExtended(laboratorium, (r, q, cb) -> cb.equal(r.get(IFOBTBestand_.laboratorium), laboratorium));
	}

	public static ExtendedSpecification<IFOBTBestand> heeftStatusDatumTussen(Range<Date> peilRange)
	{
		return bevat(peilRange, r -> r.get(IFOBTBestand_.statusDatum));
	}

}
