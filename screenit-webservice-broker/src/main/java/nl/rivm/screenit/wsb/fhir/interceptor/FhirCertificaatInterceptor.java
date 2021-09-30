package nl.rivm.screenit.wsb.fhir.interceptor;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.sourceforge.jcetaglib.lib.CertTools;

import nl.rivm.screenit.Constants;

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
			X509Certificate certificate = CertTools.getCertfromPEM(new ByteArrayInputStream(clientCertPem.getBytes()));
			String subject = certificate.getSubjectX500Principal().getName();
			String[] subjectParts = StringUtils.split(subject, ',');
			String fqdn = "";
			for (String part : subjectParts)
			{
				if (StringUtils.startsWith(part, "CN="))
				{
					fqdn = StringUtils.removeStart(part, "CN=");
					fqdn = StringUtils.trim(fqdn);
					break;
				}
			}
			LOG.info("FQDN uit Certificaat: " + fqdn);
			if (StringUtils.isBlank(fqdn) || !getFqdnStore().isFQDNValid(fqdn))
			{
				throw new AuthenticationException("No valid FQDN found in certificate.");
			}
			getFqdnStore().registerFQDN(fqdn);
		}
		catch (CertificateException | IOException e)
		{
			LOG.error(e.getMessage(), e);
			throw new AuthenticationException("No valid certificate.");
		}

		return true;
	}

	private String getClientCertPem(HttpServletRequest theRequest)
	{
		String clientPemCert = theRequest.getHeader(Constants.HTTP_HEADER_X_CLIENT_CERT);
		if (StringUtils.isBlank(clientPemCert))
		{
			clientPemCert = theRequest.getHeader(Constants.HTTP_HEADER_SSL_CLIENT_CERT);
		}

		if (StringUtils.isBlank(clientPemCert))
		{
			throw new AuthenticationException("No certificate found.");
		}

		if (!clientPemCert.contains("BEGIN CERTIFICATE"))
		{
			clientPemCert = "-----BEGIN CERTIFICATE----- " + clientPemCert + " -----END CERTIFICATE-----";
		}
		clientPemCert = clientPemCert.replaceAll(" ", "\n");
		clientPemCert = clientPemCert.replaceAll("N\nC", "N C");
		clientPemCert = clientPemCert.replaceAll("D\nC", "D C");
		LOG.debug("Client certificate: " + clientPemCert);
		return clientPemCert;
	}
}
