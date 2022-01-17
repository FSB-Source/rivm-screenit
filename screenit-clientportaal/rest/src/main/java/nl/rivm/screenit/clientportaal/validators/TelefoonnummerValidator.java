package nl.rivm.screenit.clientportaal.validators;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class TelefoonnummerValidator
{
	private final static String VAST_NUMMER_PATTERN = "^(0[0-9]{9})|(0[0-9]{2}( |-)[0-9]{7})|(0[0-9]{3}( |-)[0-9]{6})$";

	private final static String MOBIEL_NUMMER_PATTERN = "^(06( |-)?[0-9]{8})$";

	private final static String INFORMATIE_NUMMER_PATTERN = "^(0(8|9)00( |-)?\\d{4}(\\d{3})?$)$";

	private final static String BUITENLANDS_NUMMER_PATTERN = "^(\\+|00)[0-9 -]{4,15}$";

	public static boolean telefoonnummerIsCorrect(String telefoonnummer)
	{
		return telefoonnummer.matches(VAST_NUMMER_PATTERN) || telefoonnummer.matches(MOBIEL_NUMMER_PATTERN)
			|| telefoonnummer.matches(INFORMATIE_NUMMER_PATTERN) || telefoonnummer.matches(BUITENLANDS_NUMMER_PATTERN);
	}
}
