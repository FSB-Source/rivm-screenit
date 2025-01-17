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

import java.io.IOException;
import java.io.Serializable;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;
import java.util.Date;

import lombok.Value;

@Value
public class UZIpasCertInfo implements Serializable
{
	String serial;

	String oid;

	String versie;

	String uziCode;

	String type;

	String abonneeNummer;

	String agbCode;

	String rol;

	Date geldigVanaf;

	Date geldigTot;

	public UZIpasCertInfo(String serial, Date geldigVanaf, Date geldigTot)
	{
		this.serial = serial;
		String[] elementen = serial.split("-");
		this.oid = elementen[0];
		this.versie = elementen[1];
		this.uziCode = elementen[2];
		this.type = elementen[3];
		this.abonneeNummer = elementen[4];
		this.rol = elementen[5];
		this.agbCode = elementen[6];
		this.geldigVanaf = geldigVanaf;
		this.geldigTot = geldigTot;
	}

	public static UZIpasCertInfo fromCertificate(X509Certificate certificate) throws IOException, CertificateParsingException
	{
		var serial = CertificateUtil.getSerial(certificate);
		return new UZIpasCertInfo(serial, certificate.getNotBefore(), certificate.getNotAfter());
	}
}
