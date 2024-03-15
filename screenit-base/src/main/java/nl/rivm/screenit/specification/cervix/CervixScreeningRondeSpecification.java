package nl.rivm.screenit.specification.cervix;

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

import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixScreeningRondeSpecification
{
	public static Specification<CervixScreeningRonde> heeftPersoon(String bsn)
	{
		return (r, q, cb) ->
		{
			var dossier = SpecificationUtil.join(r, CervixScreeningRonde_.dossier);
			var client = SpecificationUtil.join(dossier, CervixDossier_.client);
			var persoon = SpecificationUtil.join(client, Client_.persoon);

			return cb.equal(persoon.get(GbaPersoon_.bsn), bsn);
		};
	}

}
