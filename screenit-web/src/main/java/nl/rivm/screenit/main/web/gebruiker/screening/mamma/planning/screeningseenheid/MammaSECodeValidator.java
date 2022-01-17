package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import org.apache.wicket.validation.IErrorMessageSource;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;

public class MammaSECodeValidator implements IValidator<String>
{
	public MammaSECodeValidator()
	{
	}

	@Override
	public void validate(IValidatable<String> validatable)
	{
		final String seCode = validatable.getValue();
		if (!isValide(seCode))
		{
			error(validatable);
		}
	}

	public static boolean isValide(String seCode)
	{
		if (seCode == null || seCode.length() != 6 || !seCode.substring(0, 3).equals("SE-"))
		{
			return false;
		}
		else
		{
			try
			{
				final int nummer = Integer.parseInt(seCode.substring(3, 6));
				if (nummer < 1)
				{
					return false;
				}
			}
			catch (NumberFormatException e)
			{
				return false;
			}
		}
		return true;
	}

	private void error(IValidatable<String> validatable)
	{
		validatable.error((IErrorMessageSource iErrorMessageSource) -> "Code moet zijn SE-001 of SE-002 of SE-003 etc., beginnend met twee hoofdletters");
	}
}
