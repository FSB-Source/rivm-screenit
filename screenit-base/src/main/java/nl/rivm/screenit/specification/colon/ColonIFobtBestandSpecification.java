package nl.rivm.screenit.specification.colon;

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

import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTBestand_;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)

public class ColonIFobtBestandSpecification
{

	public static Specification<IFOBTBestand> heeftStatussen(List<IFOBTBestandStatus> statussen)
	{
		return (r, q, cb) -> r.get(IFOBTBestand_.status).in(statussen);
	}

	public static Specification<IFOBTBestand> heeftBestandsnaam(String bestandsnaam)
	{
		return (r, q, cb) -> cb.equal(r.get(IFOBTBestand_.naamBestand), bestandsnaam);
	}

}
