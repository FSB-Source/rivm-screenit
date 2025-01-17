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

import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval_;
import nl.rivm.screenit.model.mamma.MammaVolgendeUitnodiging;
import nl.rivm.screenit.model.mamma.MammaVolgendeUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaVolgendeUitnodigingSpecification
{
	public static ExtendedSpecification<MammaVolgendeUitnodiging> heeftPeildatumOpOfVoorBerekendeReferentieDatum()
	{
		return (r, q, cb) ->
		{
			var intervalJoin = join(r, MammaVolgendeUitnodiging_.interval);
			return cb.lessThanOrEqualTo(r.get(MammaVolgendeUitnodiging_.peildatum), intervalJoin.get(MammaUitnodigingsinterval_.berekendeReferentieDatum));
		};
	}
}
