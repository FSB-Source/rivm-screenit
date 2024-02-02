package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.validation.IErrorMessageSource;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidationError;
import org.apache.wicket.validation.IValidator;

public class FormuliernummerValidator implements IValidator<String>
{

	private static final long serialVersionUID = 1L;

	public static final Pattern POSTCODE_NL = Pattern.compile("[1-9][0-9]{3}?[\\s]?[a-zA-Z]{2}");

	@Override
	public void validate(IValidatable<String> validatable)
	{
		String value = validatable.getValue();
		if (StringUtils.isNotBlank(value))
		{
			String formuliernummer = StringUtils.trim(value);
			formuliernummer = StringUtils.remove(formuliernummer, " ");

			if (StringUtils.length(formuliernummer) != 12 
				|| !StringUtils.startsWith(formuliernummer, "++FORMNR+") 
				|| !StringUtils.isNumeric(StringUtils.substring(formuliernummer, 9)))
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Formuliernummer niet valide [++FORMNR+<999>]";
					}
				});
			}
		}
	}

	public static boolean isValidNLPostcode(String postcode)
	{
		Matcher matcher = POSTCODE_NL.matcher(postcode);
		return matcher.matches();
	}
}
