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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBeoordelingSpecification
{
	public static Specification<Client> heeftClientLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus status)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = SpecificationUtil.join(r, Client_.mammaDossier);
			var rondeJoin = SpecificationUtil.join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
			var onderzoekJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var beoordelingJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
			return cb.equal(beoordelingJoin.get(MammaBeoordeling_.status), status);
		};
	}
}
