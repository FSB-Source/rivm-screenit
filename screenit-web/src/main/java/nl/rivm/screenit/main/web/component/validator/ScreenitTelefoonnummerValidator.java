package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.util.TelefoonnummerUtil;

import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public abstract class ScreenitTelefoonnummerValidator
{
	public static IValidator<String> mobielNederlandsNummer()
	{
		return new NederlandsMobielNummerValidator();
	}

	public static IValidator<String> alle()
	{
		return new AlleTelefoonnummerValidator();
	}

	private static final class AlleTelefoonnummerValidator implements IValidator<String>
	{
		@Override
		public void validate(IValidatable<String> validatable)
		{
			String telefoonnummer = validatable.getValue();

			if (!TelefoonnummerUtil.isCorrectTelefoonnummer(telefoonnummer))
			{
				ValidationError error = new ValidationError(this);
				error.addKey("TelefoonnummerValidator.alle");
				validatable.error(error);
			}
		}
	}

	private static final class NederlandsMobielNummerValidator implements IValidator<String>
	{
		@Override
		public void validate(IValidatable<String> validatable)
		{
			String mobielNummer = validatable.getValue();

			if (!TelefoonnummerUtil.isCorrectNederlandsMobielNummer(mobielNummer))
			{
				ValidationError error = new ValidationError(this);
				error.addKey("TelefoonnummerValidator.mobiel");
				validatable.error(error);
			}
		}
	}
}
