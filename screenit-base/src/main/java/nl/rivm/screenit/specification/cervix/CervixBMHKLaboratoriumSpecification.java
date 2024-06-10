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

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BMHKLaboratorium_;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixBMHKLaboratoriumSpecification
{
	public static Specification<BMHKLaboratorium> heeftZInstrumentNames(String zInstrumentName)
	{
		return (r, q, cb) ->
		{
			var instrumentNames = SpecificationUtil.join(r, BMHKLaboratorium_.instrumentNames);

			return cb.equal(instrumentNames, zInstrumentName);
		};
	}

	public static Specification<BMHKLaboratorium> heeftUserIdScanner(String userIdScanner)
	{
		return (r, q, cb) ->
		{
			var userIdScannerPath = SpecificationUtil.join(r, BMHKLaboratorium_.userIdScanners);

			return cb.equal(userIdScannerPath, userIdScanner);
		};
	}
}
