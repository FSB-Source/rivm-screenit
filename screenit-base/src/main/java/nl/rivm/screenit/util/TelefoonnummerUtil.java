package nl.rivm.screenit.util;

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

import org.apache.commons.lang.StringUtils;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class TelefoonnummerUtil
{
	public static final String VAST_NL_NUMMER_PATTERN = "^(0[0-9]{9})|(0[0-9]{2}[- ][0-9]{7})|(0[0-9]{3}[- ][0-9]{6})$";

	public static final String MOBIEL_NL_NUMMER_PATTERN = "^(06|\\+316|00316)[- ]?[0-9]{8}$";

	public static final String NL_INFORMATIE_NUMMER_PATTERN = "^0[89]00[- ]?\\d{4}(\\d{3})?$";

	public static final String BUITENLANDS_NUMMER_PATTERN = "^(\\+|00)[0-9 -]{4,15}$";

	public static boolean isCorrectTelefoonnummer(String telefoonnummer)
	{
		return StringUtils.isNotBlank(telefoonnummer)
			&& (telefoonnummer.matches(TelefoonnummerUtil.VAST_NL_NUMMER_PATTERN) || telefoonnummer.matches(TelefoonnummerUtil.MOBIEL_NL_NUMMER_PATTERN) ||
			telefoonnummer.matches(TelefoonnummerUtil.NL_INFORMATIE_NUMMER_PATTERN) || telefoonnummer.matches(TelefoonnummerUtil.BUITENLANDS_NUMMER_PATTERN));
	}

	public static boolean isCorrectNederlandsMobielNummer(String telefoonnummer)
	{
		return StringUtils.isNotBlank(telefoonnummer) && telefoonnummer.matches(TelefoonnummerUtil.MOBIEL_NL_NUMMER_PATTERN);
	}
}
