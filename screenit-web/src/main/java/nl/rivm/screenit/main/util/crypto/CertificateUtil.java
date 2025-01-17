package nl.rivm.screenit.main.util.crypto;

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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERIA5String;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CertificateUtil
{
	private static final String OID_SERIAL = "2.5.5.5";

	public static List<X509Certificate> decodeCertificates(final String certificates) throws GeneralSecurityException
	{

		return decodeCertificates(certificates, true);
	}

	static List<X509Certificate> decodeCertificates(final String certificatesString, final boolean checkValidity) throws GeneralSecurityException
	{
		var decoder = Base64.getDecoder();
		var certificates = certificatesString.split("\\|");

		var result = new ArrayList<X509Certificate>(certificates.length);

		for (var cert : certificates)
		{
			var encodedCertificate = decoder.decode(cert);
			var inputStream = new ByteArrayInputStream(encodedCertificate);
			var decodedCertificate = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(inputStream);

			if (checkValidity)
			{
				decodedCertificate.checkValidity();
			}
			result.add(decodedCertificate);
		}

		return result;
	}

	public static String getSerial(X509Certificate certificate) throws IOException, CertificateParsingException
	{
		var alternativeNames = certificate.getSubjectAlternativeNames();

		if (alternativeNames == null)
		{
			throw new IllegalArgumentException("Certificate does not contain Subject Alternative Names");
		}

		return parseAlternativeNamesForDERIA5String(alternativeNames);
	}

	private static String parseAlternativeNamesForDERIA5String(Collection<List<?>> alternativeNames) throws IOException
	{
		for (var alternativeNameObjects : alternativeNames)
		{
			for (var nameObject : alternativeNameObjects)
			{
				if (nameObject instanceof byte[] nameObjectBytes)
				{
					var byteArrayInputStream = new ByteArrayInputStream(nameObjectBytes);
					var inputStreamASN = new ASN1InputStream(byteArrayInputStream);
					var asn1Primitive = inputStreamASN.readObject();

					if (asn1Primitive instanceof ASN1Sequence asn1Sequence)
					{
						var asn1SequenceObjects = asn1Sequence.getObjects();
						var isOIDSerial = false;
						while (asn1SequenceObjects.hasMoreElements())
						{
							var asn1Object = asn1SequenceObjects.nextElement();
							if (asn1Object instanceof ASN1ObjectIdentifier objectIdentifier)
							{
								isOIDSerial = OID_SERIAL.equals(objectIdentifier.getId());
							}
							else if (isOIDSerial && asn1Object instanceof ASN1TaggedObject taggedObject)
							{
								return extractDERIA5StringFromTaggedObject(taggedObject);
							}
						}
					}
				}
			}
		}
		return null;
	}

	private static String extractDERIA5StringFromTaggedObject(ASN1TaggedObject taggedObject)
	{
		while (true)
		{
			var baseObject = taggedObject.getBaseObject();
			if (baseObject instanceof ASN1TaggedObject asn1TaggedObject)
			{
				taggedObject = asn1TaggedObject;
			}
			else if (baseObject instanceof DERIA5String deria5String)
			{
				return deria5String.getString();
			}
			else
			{
				break;
			}
		}
		return null;
	}
}
