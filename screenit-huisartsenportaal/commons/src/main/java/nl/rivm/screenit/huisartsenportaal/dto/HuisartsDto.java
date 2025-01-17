package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
 * %%
 * Copyright (C) 2016 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
@Getter
@Setter
public class HuisartsDto extends AbstractDtoReferenceObject
{
	@NotNull(message = "AGB-code is verplicht.")
	@Size(min = 8)
	private String agbcode;

	@NotNull(message = "E-mail is verplicht.")
	@Pattern(regexp = "^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*((\\.[A-Za-z]{2,}){1}$)", message = "E-mail voldoet niet aan het verplichte formaat")
	private String email;

	private String aanhef;

	@NotNull(message = "Achternaam is verplicht.")
	@Size(max = 100)
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F\\-'\\s]*$", message = "Achternaam mag alleen letters, spaties en koppeltekens bevatten")
	private String achternaam;

	@Size(max = 20, message = "Tussenvoegsel mag maximaal 20 tekens bevatten")
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F'\\s]*$", message = "Tussenvoegsel mag alleen letters en spaties bevatten")
	private String tussenvoegsel;

	@Size(max = 20, message = "'Voorletters' mag maximaal 20 tekens bevatten")
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F.]*$", message = "'Voorletters' mag alleen letters en punten bevatten")
	private String voorletters;

	@Size(max = 25, message = "Telefoonnummer mag maximaal 25 tekens bevatten")
	private String telefoon;

	@JsonIgnore
	private String inlogCode;

	@JsonIgnore
	private String aanmeldStatus;

	@JsonIgnore
	private String status;

	private String username;

	private String wachtwoord;

	@JsonIgnore
	private Boolean actief;

	@NotNull(message = "U dient de zakelijke voorwaarden te accorderen.")
	private Boolean overeenkomst;

	@Pattern(
		regexp = "^(([a-zA-Z0-9_\\-\\.]+)@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.)|(([a-zA-Z0-9\\-]+\\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\\]?)(\\s*(;|,)\\s*|\\s*$))*$",
		message = "Eén of meerdere van de e-mailadressen voldoet niet aan het verplichte formaat")
	private String extraEmails;

	private Date overeenkomstGeaccordeerdDatum;

	@Valid
	@NotNull
	private AdresDto postadres = new AdresDto();

}
