package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.function.UnaryOperator;

import lombok.Getter;

@Getter
public class ValidatieException extends Exception
{
	private final Serializable[] formatArguments;

	private final String messageKey;

	public ValidatieException(String messageKey)
	{
		super(messageKey);
		this.messageKey = messageKey;
		this.formatArguments = null;
	}

	public ValidatieException(String messageKey, Serializable... args)
	{
		super(messageKey);
		this.messageKey = messageKey;
		this.formatArguments = args;
	}

	@Override
	public boolean equals(Object obj)
	{
		var other = (ValidatieException) obj;
		if (other == null)
		{
			return false;
		}

		if (getClass() != obj.getClass())
		{
			return false;
		}

		return (other.getMessage().equalsIgnoreCase(getMessage()) && Arrays.equals(other.getFormatArguments(), getFormatArguments()));
	}

	@Override
	public int hashCode()
	{
		return messageKey.hashCode() + Arrays.hashCode(formatArguments);
	}

	public String getFormattedMessage(UnaryOperator<String> getString)
	{
		var message = getString.apply(messageKey);
		if (formatArguments != null)
		{
			message = String.format(message, (Object[]) formatArguments);
		}
		return message;
	}
}
