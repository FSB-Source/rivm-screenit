package nl.rivm.screenit.model;

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

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum RedenGbaVraag
{
	ONJUIST_ADRES("Onjuist adres"),
	ONJUISTE_PERSOONSGEGEVENS("Onjuiste persoonsgegevens"),

	MUTATIEBERICHT_ONBEKENDE_CLIENT("Mutatiebericht ontvangen voor onbekende client"),

	BEZWAAR("Bezwaar uitwisseling BRP gemaakt"), 
	BEZWAAR_INGETROKKEN("Bezwaar uitwisseling BRP ingetrokken"), 

	AFGEMELD("Afgemeld voor alle bevolkingsonderzoeken binnen leeftijddoelgroep"), 
	AANGEMELD("Heraanmelding met door BVO ingetrokken indicatie"), 

	BOVENGRENS_LEEFTIJD("Bovengrens leeftijd BRP indicatie bereikt"), 
	SELECTIEBLOKKADE("Geen reactie na selectieblokkade"), 

	BRIEF_VERSTUREN("Brief versturen bij door BVO ingetrokken indicatie"), 

	ONVERWACHT_INDICATIE_VERWIJDERD("Onverwacht indicatie-verwijderd-bericht ontvangen"), 
	;

	private final String naam;

	@Override
	public String toString()
	{
		return this.naam;
	}

}
