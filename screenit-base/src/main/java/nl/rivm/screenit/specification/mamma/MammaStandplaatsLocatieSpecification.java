package nl.rivm.screenit.specification.mamma;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.TijdelijkAdres_;
import nl.rivm.screenit.model.UploadDocument_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaStandplaatsLocatieSpecification
{
	public static Specification<MammaStandplaatsLocatie> heeftGeenPostcodeCoordinaten()
	{
		return (r, q, cb) -> cb.isNull(r.get(TijdelijkAdres_.postcodeCoordinaten));
	}

	public static ExtendedSpecification<MammaStandplaatsLocatie> heeftBijlageOfApartPrinten()
	{
		return heeftBrievenApartPrinten().or(heeftActieveBijlage());
	}

	public static ExtendedSpecification<MammaStandplaatsLocatie> heeftGeenBijlageOfApartPrinten()
	{
		return heeftNietBrievenApartPrinten().and(not(heeftActieveBijlage()));
	}

	private static ExtendedSpecification<MammaStandplaatsLocatie> heeftActieveBijlage()
	{
		return (r, q, cb) ->
		{
			var bijlageJoin = join(r, MammaStandplaatsLocatie_.standplaatsLocatieBijlage, LEFT);
			return cb.and(
				cb.isNotNull(r.get(MammaStandplaatsLocatie_.standplaatsLocatieBijlage)),
				cb.isTrue(bijlageJoin.get(UploadDocument_.actief)));
		};
	}

	private static ExtendedSpecification<MammaStandplaatsLocatie> heeftBrievenApartPrinten()
	{
		return (r, q, cb) -> cb.isTrue(r.get(MammaStandplaatsLocatie_.brievenApartPrinten));
	}

	private static ExtendedSpecification<MammaStandplaatsLocatie> heeftNietBrievenApartPrinten()
	{
		return (r, q, cb) -> cb.isFalse(cb.coalesce(r.get(MammaStandplaatsLocatie_.brievenApartPrinten), false));
	}
}
