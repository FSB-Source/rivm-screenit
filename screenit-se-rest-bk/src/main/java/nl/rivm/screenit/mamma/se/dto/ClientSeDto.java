package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.mamma.se.dto.onderzoek.VorigOnderzoekDto;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

@Getter
@Setter
public class ClientSeDto extends SeDto
{
	private Integer jaarLaatsteVerwijzing;

	private String voorletters;

	private String geboorteTussenvoegsel;

	private String geboorteAchternaam;

	private String aanspreekTussenvoegselEnAchternaam;

	private String bsn;

	private LocalDate geboortedatum;

	private String geslacht;

	private AdresSeDto adres;

	private String emailadres;

	private AdresSeDto tijdelijkGbaAdres;

	private TijdelijkAdresSeDto tijdelijkAdres;

	private String telefoonnummer1;

	private String telefoonnummer2;

	private MammaDoelgroep doelgroep;

	private boolean inTehuis;

	private String dubbeleTijdReden;

	private List<VorigOnderzoekDto> vorigeOnderzoeken;
}
