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

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuis_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaTehuisSpecification
{
	public static Specification<MammaTehuis> filterNaam(String naam)
	{
		return skipWhenEmpty(naam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(MammaTehuis_.naam), naam));
	}

	public static Specification<MammaTehuis> filterStandplaats(MammaStandplaats standplaats)
	{
		return skipWhenNull(standplaats, (r, q, cb) -> cb.equal(r.get(MammaTehuis_.standplaats), standplaats));
	}

	public static Specification<MammaTehuis> heeftStandplaatsInRegio(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, (r, q, cb) ->
		{
			var standplaatsJoin = join(r, MammaTehuis_.standplaats);
			return cb.equal(standplaatsJoin.get(MammaStandplaats_.regio), screeningOrganisatie);
		});
	}

	public static Specification<MammaTehuis> filterActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(MammaTehuis_.actief), actief));
	}
}
