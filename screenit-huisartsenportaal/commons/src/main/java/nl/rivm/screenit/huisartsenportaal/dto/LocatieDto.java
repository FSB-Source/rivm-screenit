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

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.ICervixHuisartsLocatie;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
@Getter
@Setter
public class LocatieDto extends AbstractDtoReferenceObject implements ICervixHuisartsLocatie
{
	public static final String EMPTY_VALUE = "<empty>";

	@NotNull(message = "IBAN is verplicht.")
	@Size(max = 34)
	private String iban;

	@NotNull(message = "Tenaamstelling is verplicht.")
	@Size(max = 70, message = "Tenaamstelling is te lang.")
	private String ibanTenaamstelling;

	@NotNull(message = "Locatienaam is verplicht.")
	@Size(max = 70, message = "Locatienaam is te lang.")
	private String naam;

	@Valid
	@NotNull(message = "Locatieadres is verplicht.")
	private AdresDto locatieAdres = new AdresDto();

	@NotNull
	private String status;

	private Boolean herzendVerificatieMail = null;

	@NotNull(message = "Zorgmail klantnummer is verplicht.")
	@Pattern(regexp = "[0-9]{9,9}", message = "Zorgmail klantnummer voldoet niet aan het verplichte formaat")
	private String zorgmailklantnummer;

	private Long huisartsId;

	private String verificatieCode;

	private Boolean moetVerifierenVoorActivatie = false;

}
