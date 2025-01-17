package nl.rivm.screenit.specification.cervix;

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

import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag_;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixLabformulierAanvraagSpecification
{
	public static ExtendedSpecification<CervixLabformulierAanvraag> heeftNietStatus(CervixLabformulierAanvraagStatus status)
	{
		return (r, q, cb) -> cb.notEqual(r.get(CervixLabformulierAanvraag_.status), status);
	}

	public static ExtendedSpecification<CervixLabformulierAanvraag> heeftStatus(CervixLabformulierAanvraagStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixLabformulierAanvraag_.status), status);
	}

	public static ExtendedSpecification<CervixLabformulierAanvraag> getAanvragenVanHuisarts(CervixHuisarts huisarts)
	{
		return heeftNietStatus(CervixLabformulierAanvraagStatus.VERWIJDERD)
			.and(CervixHuisartsLocatieSpecification.heeftHuisarts(huisarts).with(CervixLabformulierAanvraag_.huisartsLocatie));
	}
}
