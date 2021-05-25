package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class VoornaamValidator implements IValidator<String>
{

	private static final long serialVersionUID = 1L;

	public static final Pattern PATTERN = Pattern.compile("([a-zA-Z\\u00C0-\\u024F|\\s])*");

	@Override
	public void validate(IValidatable<String> validatable)
	{
		if (validatable != null && StringUtils.isNotBlank(validatable.getValue()))
		{

			String voornaam = validatable.getValue();

			if (StringUtils.length(voornaam) > 20)
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Voornaam mag maximaal 20 tekens bevatten";
					}
				});
			}
			else if (!isValid(voornaam))
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Voornaam is niet valide";
					}
				});
			}
		}
	}

	public static boolean isValid(String input)
	{
		Matcher matcher = PATTERN.matcher(input);
		return matcher.matches();
	}

}
