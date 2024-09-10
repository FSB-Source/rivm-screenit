package nl.rivm.screenit.specification.mamma;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaDownloadOnderzoekenVerzoekSpecification
{
	public static Specification<MammaDownloadOnderzoekenVerzoek> heeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return (r, q, cb) ->
		{
			var downloadOnderzoekJoin = SpecificationUtil.join(r, MammaDownloadOnderzoekenVerzoek_.onderzoeken);
			var onderzoekJoin = SpecificationUtil.join(downloadOnderzoekJoin, MammaDownloadOnderzoek_.onderzoek);
			var afspraakJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.uitnodiging);
			return cb.equal(uitnodigingJoin.get(MammaUitnodiging_.screeningRonde), screeningRonde);
		};
	}
}
