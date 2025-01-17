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

import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

public final class RestApiFactory
{
	private RestApiFactory()
	{

	}

	public static final RestTemplate create()
	{
		return create(null);
	}

	public static final RestTemplate create(Integer readTimeout)
	{
		LoggingRequestInterceptor loggingInterceptor = new LoggingRequestInterceptor();
		RestTemplate restTemplate = null;
		if (readTimeout != null)
		{
			HttpComponentsClientHttpRequestFactory httpRequestFactory = new HttpComponentsClientHttpRequestFactory();
			httpRequestFactory.setReadTimeout(readTimeout);
			restTemplate = new RestTemplate(httpRequestFactory);
		}
		else
		{
			restTemplate = new RestTemplate();
		}
		restTemplate.setErrorHandler(new ScreenitRestErrorHandler());
		restTemplate.getInterceptors().add(loggingInterceptor);
		return restTemplate;
	}
}
