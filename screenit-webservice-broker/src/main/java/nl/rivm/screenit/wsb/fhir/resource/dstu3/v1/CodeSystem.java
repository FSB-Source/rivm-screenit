package nl.rivm.screenit.wsb.fhir.resource.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum CodeSystem
{

	AGB("http://fhir.nl/fhir/NamingSystem/agb-z", null),
	BSN("http://fhir.nl/fhir/NamingSystem/bsn", null),
	CONTROLELETTERS("http://landelijkescreening.nl/fhir/NamingSystem/controleletters", "BVOBMHK_VERIFICATIONCODE"),
	MONSTER_ID("http://landelijkescreening.nl/fhir/NamingSystem/monsterid", "BVOBMHK_SPECIMENID"),

	DATUM_UITSTRIJKJE(null, "BVOBMHK_DATEOFPAPSMEAR"),
	KLACHTEN(null, "BVOBMHK_COMPLAINTSYES"),
	KLACHTEN_VRIJE_TEKST(null, "BVOBMHK_COMPLAINTSOTHER"),

	MENSTRUATIE(null, "BVOBMHK_PERIOD"),
	DATUM_LAATSTE_MENSTRUATIE(null, "BVOBMHK_DATEOFLASTPERIOD"),

	ANTICONCEPTIE(null, "BVOBMHK_BIRTHCONTROL"),

	GEBRUIK_HORMONEN(null, "BVOBMHK_HORMONEUSAGEYES"),
	GEBRUIK_HORMONEN_VRIJE_TEKST(null, "BVOBMHK_HORMONEUSAGEOTHER"),

	ASPECT_CERVIX(null, "BVOBMHK_ASPECTCERVIX"),
	ASPECT_CERVIX_VRIJE_TEKST(null, "BVOBMHK_ASPECTCERVIXABNORMAL"),

	OPMERKINGEN_TEKST(null, "BVOBMHK_NOTES");

	private final String url;

	private final String code;

	CodeSystem(String url,
		String code)
	{
		this.url = url;
		this.code = code;
	}

	public String getUrl()
	{
		return url;
	}

	public String getCode()
	{
		return code;
	}

	@Override
	public String toString()
	{
		return getUrl();
	}

}
