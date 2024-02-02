package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import org.apache.commons.lang.StringUtils;
import org.springframework.validation.Errors;

public abstract class BaseWachtwoordValidator<T> extends BaseValidator<T>
{

	private static final String PASSWORD_REGEX = "^(?=.*?[A-Z])(?=(.*[a-z]){1,})(?=(.*[\\d]){1,})(?=(.*[\\W]){1,})(?!.*\\s).{12,}$";

	private static final String ALFABET = "abcdefghijklmnopqrstuvwxyz";

	private static final String TOETSENBORD = "qwertyuiopasdfghjklzxcvbnm";

	void controleerGebruikersnaamInWachtwoord(String gebruikersnaam, String wachtwoord, Errors errors)
	{
		if (StringUtils.containsIgnoreCase(wachtwoord, gebruikersnaam))
		{
			errors.reject("error.password.containsusername", "Het wachtwoord mag niet de gebruikersnaam bevatten.");
		}
	}

	protected void controleerTekenEisen(String wachtwoord, Errors errors)
	{
		if (!wachtwoord.matches(PASSWORD_REGEX))
		{
			errors.reject("error.password.not.valid", "Zorg dat het wachtwoord minimaal 1 hoofdletter, 1 kleine letter, 1 nummer en 1 speciaal teken bevat.");
		}
		if (wachtwoord.length() < 12)
		{
			errors.reject("error.password.length.min", "Wachtwoord moet minstens 12 tekens bevatten.");
		}
		if (wachtwoord.length() > 255)
		{
			errors.reject("error.password.length.max", "Wachtwoord mag maximaal 255 tekens bevatten.");
		}
		String alfabetSequence = zoekSequenceInWachtwoord(ALFABET, wachtwoord.toLowerCase(), 5);
		if (alfabetSequence != null)
		{
			errors.reject("error.password.alfabet", "Wachtwoord bevat tekenreeks die niet is toegestaan: " + alfabetSequence);
		}
		String toetsenbordSequence = zoekSequenceInWachtwoord(TOETSENBORD, wachtwoord.toLowerCase(), 5);
		if (toetsenbordSequence != null)
		{
			errors.reject("error.password.toetsenbord", "Wachtwoord bevat tekenreeks die niet is toegestaan: " + toetsenbordSequence);
		}
	}

	private String zoekSequenceInWachtwoord(String pattern, String wachtwoord, int sequenceLengte)
	{
		if (wachtwoord.length() < sequenceLengte)
		{
			return null;
		}
		for (int i = 0; i <= wachtwoord.length() - sequenceLengte; i++)
		{
			int indexInPattern = pattern.indexOf(wachtwoord.charAt(i));
			if (indexInPattern >= 0)
			{
				for (int j = 0; j < sequenceLengte; j++)
				{
					if (pattern.length() - 1 < indexInPattern + j || wachtwoord.charAt(i + j) != pattern.charAt(indexInPattern + j))
					{
						break;
					}
					else if (j == sequenceLengte - 1)
					{
						return wachtwoord.substring(i, i + sequenceLengte);
					}
				}
			}
		}
		return null;
	}

}
