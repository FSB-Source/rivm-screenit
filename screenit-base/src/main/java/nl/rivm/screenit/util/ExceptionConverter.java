package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.ExceptieOmschrijving;

import org.hibernate.AssertionFailure;
import org.hibernate.exception.GenericJDBCException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.hibernate5.HibernateJdbcException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class ExceptionConverter
{
	private static final Logger LOGGER = LoggerFactory.getLogger(ExceptionConverter.class);

	private static ObjectMapper OBJECTMAPPER = new ObjectMapper();

	private ExceptionConverter()
	{
	}

	public static String getGebruikerMeldingUitTriggerMessage(RuntimeException e)
	{
		String uiMessage = null;
		Throwable cause = null;
		GenericJDBCException jdbException = null;
		if (e instanceof HibernateJdbcException)
		{
			cause = ((HibernateJdbcException) e).getCause();
		}
		else if (e instanceof AssertionFailure)
		{

			cause = ((AssertionFailure) e).getCause();
			cause = cause != null ? cause.getCause() : null;
		}
		else
		{
			cause = e;
		}
		if (cause instanceof GenericJDBCException)
		{
			jdbException = (GenericJDBCException) cause;
			if (jdbException.getSQLException() != null)
			{
				String message = jdbException.getSQLException().getMessage();
				int startTriggerMessage = message.indexOf("[" + Constants.EXCEPTION_GEBRUIKERMELDING_MARKER);
				int endTriggerMessage = message.indexOf(Constants.EXCEPTION_GEBRUIKERMELDING_MARKER + "]");
				if (startTriggerMessage > 0 && endTriggerMessage > startTriggerMessage)
				{
					uiMessage = " "
						+ message.substring(startTriggerMessage + Constants.EXCEPTION_GEBRUIKERMELDING_MARKER.length() + 1, endTriggerMessage).trim();
				}
			}
		}
		else if (cause instanceof IllegalStateException)
		{
			uiMessage = " " + e.getMessage();
		}
		if (uiMessage == null)
		{
			uiMessage = " Onbekende fout.";
			LOGGER.error("Onverwachte jdbc fout.", e);
		}
		return uiMessage;
	}

	public static String convertExceptionToJson(RuntimeException e, String keyValue)
	{
		ExceptieOmschrijving exceptieOmschrijving = new ExceptieOmschrijving(keyValue, ExceptionConverter.getGebruikerMeldingUitTriggerMessage(e));
		try
		{
			return OBJECTMAPPER.writeValueAsString(exceptieOmschrijving);
		}
		catch (JsonProcessingException x)
		{
			throw new IllegalStateException("Cannot convert Dto to Json", x);
		}
	}
}
