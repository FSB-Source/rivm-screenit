package nl.rivm.screenit.clientportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

@Getter
@Setter
@NoArgsConstructor
public class ClientDto extends ClientportaalBaseDto
{

	private String voorletters;

	private String aanhef;

	private String aanspreekTussenvoegselEnAchternaam;

	private String bsn;

	private String geboortedatumDisplay;

	private Geslacht geslacht;

	private String adresTekst;

	private String emailadres;

	private TijdelijkAdresDto tijdelijkAdres;

	private String tijdelijkAdresTekst;

	private String telefoonnummer1;

	private String telefoonnummer2;

	private Boolean vertrokkenUitNederland;
}
