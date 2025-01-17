package nl.rivm.screenit.wsb.fhir.interceptor;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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
import java.security.cert.CertificateException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.wsb.tools.CertTools;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.fhir.interceptor.api.Hook;
import ca.uhn.fhir.interceptor.api.Interceptor;
import ca.uhn.fhir.interceptor.api.Pointcut;
import ca.uhn.fhir.rest.server.exceptions.AuthenticationException;

@Interceptor
public class FhirCertificaatInterceptor extends FQDNAwareInterceptor
{

	private static final Logger LOG = LoggerFactory.getLogger(FhirCertificaatInterceptor.class);

	@Hook(Pointcut.SERVER_INCOMING_REQUEST_PRE_PROCESSED)
	public boolean incomingRequestPreProcessed(HttpServletRequest theRequest, HttpServletResponse theResponse)
	{
		if (!getFqdnStore().isFQDNCheckNeeded())
		{
			getFqdnStore().registerFQDN("Niet bepaald");
			return true;
		}

		String clientCertPem = getClientCertPem(theRequest);
		try
		{
			String fqdn = CertTools.getFQDNFromCert(clientCertPem);
			if (StringUtils.isBlank(fqdn) || !getFqdnStore().isFQDNValid(fqdn))
			{
				throw new AuthenticationException("No valid FQDN found in certificate.");
			}
			getFqdnStore().registerFQDN(fqdn);
		}
		catch (IOException | CertificateException e)
		{
			LOG.error(e.getMessage(), e);
			throw new AuthenticationException("No valid certificate.");
		}

		return true;
	}

	private String getClientCertPem(HttpServletRequest theRequest)
	{
		String clientCertPem = theRequest.getHeader(Constants.HTTP_HEADER_X_CLIENT_CERT);
		if (StringUtils.isBlank(clientCertPem))
		{
			clientCertPem = theRequest.getHeader(Constants.HTTP_HEADER_SSL_CLIENT_CERT);
		}

		if (StringUtils.isBlank(clientCertPem))
		{
			throw new AuthenticationException("No certificate found.");
		}

		clientCertPem = CertTools.correctCertPem(clientCertPem);

		LOG.debug("Client certificate: " + clientCertPem);
		return clientCertPem;
	}
}
