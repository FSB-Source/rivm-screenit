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
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixVerslagSpecification
{
	public static Specification<CervixCytologieVerslag> heeftClientId(Long clientId)
	{
		return heeftId(clientId).with(r ->
		{
			var screeningRondeJoin = join(r, CervixVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, CervixScreeningRonde_.dossier);
			return join(dossierJoin, CervixDossier_.client);
		});
	}

	public static Specification<CervixCytologieVerslag> heeftType(VerslagType type)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixVerslag_.type), type);
	}
}
