package nl.rivm.screenit.util.rest;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;

import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.DefaultResponseErrorHandler;

public class ScreenitRestErrorHandler extends DefaultResponseErrorHandler
{
	private static ThreadLocal<IRestErrorHandlerCallback> callbackHandler = new ThreadLocal<>();

	@Override
	protected void handleError(ClientHttpResponse response, HttpStatus statusCode) throws IOException
	{
		if (callbackHandler.get() != null)
		{
			callbackHandler.get().handleRestClientError(statusCode);
		}
		super.handleError(response, statusCode);
	}

	public static void set(IRestErrorHandlerCallback callbackHandler)
	{
		ScreenitRestErrorHandler.callbackHandler.set(callbackHandler);
	}
}
