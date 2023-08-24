package nl.rivm.screenit.model.enums;

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

import nl.rivm.screenit.model.CodeboekDoc;

@CodeboekDoc("GBA heet tegenwoordig BRP (= Basis Registratie Personen). Dit is onze enige bron van clientgegevens.")
public enum GbaStatus
{
	GEEN_INDICATIE,

	INDICATIE_VERWIJDERD,

	@CodeboekDoc("Als een client bijvoorbeeld meldt dat een brief niet is aangekomen, vragen we automatisch extra " +
		"informatie aan de BRP. Dan doen we tijdelijk geen communicatie met de client.")
	INDICATIE_AANGEVRAAGD,

	INDICATIE_AANWEZIG,

	PUNT_ADRES,

	@CodeboekDoc("Er is een NG01 ontvangen voor deze client, wat betekent dat er niks meer mee gedaan mag worden. " +
		"NG01 is een van de 15 a 20 typen records in het bestand waarmee we gegevens uitwisselen met de BRP.")
	AFGEVOERD,

	@CodeboekDoc("Er is een bezwaar gemaakt op het BRP. Deze client ontvangt geen updates meer van GBA.")
	BEZWAAR
}
