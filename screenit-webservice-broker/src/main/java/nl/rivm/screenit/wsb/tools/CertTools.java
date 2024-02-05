package nl.rivm.screenit.wsb.tools;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import org.apache.commons.lang.StringUtils;
import org.bouncycastle.util.io.pem.PemObject;
import org.bouncycastle.util.io.pem.PemReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CertTools
{
	private static final Logger LOG = LoggerFactory.getLogger(CertTools.class);

	private static final CertificateFactory CERTIFICATE_FACTORY = createCertificateFactory();

	private static CertificateFactory createCertificateFactory()
	{
		try
		{
			return CertificateFactory.getInstance("X.509");
		}
		catch (CertificateException e)
		{
			throw new IllegalStateException("Kon CertificateFactory niet aanmaken", e);
		}
	}

	public static String getFQDNFromCert(String pem) throws IOException, CertificateException
	{
		try (final PemReader certReader = new PemReader(new StringReader(pem)))
		{
			final PemObject certPem = certReader.readPemObject();
			final ByteArrayInputStream inputStream = new ByteArrayInputStream(certPem.getContent());
			final X509Certificate certificate = (X509Certificate) CERTIFICATE_FACTORY.generateCertificate(inputStream);
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
			return fqdn;
		}
	}

	public static String correctCertPem(String pem)
	{
		if (!pem.contains("BEGIN CERTIFICATE"))
		{
			pem = "-----BEGIN CERTIFICATE----- " + pem + " -----END CERTIFICATE-----";
		}
		pem = pem.replaceAll(" ", "\n");
		pem = pem.replaceAll("N\nC", "N C");
		pem = pem.replaceAll("D\nC", "D C");
		return pem;
	}
}
