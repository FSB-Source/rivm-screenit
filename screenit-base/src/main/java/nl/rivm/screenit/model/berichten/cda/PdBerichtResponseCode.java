
package nl.rivm.screenit.model.berichten.cda;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum PdBerichtResponseCode
{

	OK("OK"),

	CLIENT_UNK("In bericht met id %s staat een onbekende client met bsn %s."),

	CLIENT_BEZWAAR("Client met bsn %s heeft bezwaar gemaakt op gegevensuitwisseling, dit bericht mag niet worden ingelezen."),

	REEDS_CORRECT_VERWERKT("Bericht met id %s is al eerder ontvangen en succesvol verwerkt."),

	ONGELDIGE_VERSIE("Van het bericht met setId %s is reeds een versie >=%s ontvangen."),

	SYSTEM_ERROR("Er is een fout opgetreden in de broker bij verwerken van bericht"),

	CDA_SOAP_INCONSISTENT("%s (%s) in SOAP is niet gelijk aan %s (%s) in CDA.");

	private final String omschrijving;

	PdBerichtResponseCode(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	@Override
	public String toString()
	{
		return omschrijving;
	}
}
