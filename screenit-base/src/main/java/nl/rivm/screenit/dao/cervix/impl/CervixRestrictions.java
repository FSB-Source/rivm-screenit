package nl.rivm.screenit.dao.cervix.impl;

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

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Restrictions;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixRestrictions
{
	public static Conjunction createLocatieCompleetRestriction(String locatieAlias)
	{
		locatieAlias = ScreenitRestrictions.fixAlias(locatieAlias);
		return Restrictions.and(
			Restrictions.ne(locatieAlias + "naam", ""),
			Restrictions.ne(locatieAlias + "iban", ""),
			Restrictions.ne(locatieAlias + "ibanTenaamstelling", ""),
			Restrictions.ne(locatieAlias + "zorgmailklantnummer", ""),
			Restrictions.ne(locatieAlias + "naam", LocatieDto.EMPTY_VALUE),
			Restrictions.ne(locatieAlias + "iban", LocatieDto.EMPTY_VALUE),
			Restrictions.ne(locatieAlias + "ibanTenaamstelling", LocatieDto.EMPTY_VALUE));
	}
}
