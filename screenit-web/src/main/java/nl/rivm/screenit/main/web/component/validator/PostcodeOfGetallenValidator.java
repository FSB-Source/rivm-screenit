package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class PostcodeOfGetallenValidator implements IValidator<String>
{

	private static final long serialVersionUID = 1L;

	private boolean alleenCijfersToegestaan;

	public PostcodeOfGetallenValidator()
	{
		this(false);
	}

	public PostcodeOfGetallenValidator(boolean alleenCijfersToegestaan)
	{
		this.alleenCijfersToegestaan = alleenCijfersToegestaan;

	}

	public static final Pattern POSTCODE_NL = Pattern.compile("\\A[1-9][0-9]{3}[ ]?([A-RT-Za-rt-z][A-Za-z]|[sS][BCbcE-Re-rT-Zt-z])\\z");

	@Override
	public void validate(IValidatable<String> validatable)
	{
		if (validatable != null && StringUtils.isNotBlank(validatable.getValue()))
		{
			String postcode = StringUtils.trim(validatable.getValue());
			postcode = StringUtils.remove(postcode, " ");

			if (StringUtils.length(postcode) == 4 && (!StringUtils.isNumeric(postcode) || !alleenCijfersToegestaan))
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Postcode niet valide";
					}
				});
			}
			else if ((StringUtils.length(postcode) == 6 || StringUtils.length(postcode) == 7) && !isValidNLPostcode(postcode))
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Postcode niet valide";
					}
				});
			}
			else if (StringUtils.length(postcode) != 4 && StringUtils.length(postcode) != 6 && StringUtils.length(postcode) != 7)
			{
				validatable.error(new IValidationError()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Serializable getErrorMessage(IErrorMessageSource messageSource)
					{
						return "Postcode niet valide";
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
