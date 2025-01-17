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

import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.mamma.se.dto.onderzoek.MammografieSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;

@Getter
@Setter
public class AfspraakSeDto extends SeDto
{
	private LocalDateTime vanaf;

	private ClientSeDto client;

	private AfspraakStatusSe status;

	private Long uitnodigingsNr; 

	private MammaIdentificatiesoort identificatiesoort;

	private String identificatienummer;

	private boolean bezwaarAangevraagd;

	private long aantalOproepen;

	private long aantalOpgekomen;

	private OnderzoekSeDto huidigOnderzoek;

	private MammografieSeDto mammografie;

	private SignalerenSeDto signaleren;

	private Long huisartsId;

	private MammaGeenHuisartsOption geenHuisartsOptie;

	private boolean doorgevoerd;

	private boolean centralAvailable;

	private boolean eerderOnderbrokenInZelfdeRonde;

	private MammaBeoordelingOpschortenReden eerdereOpschortenReden;

	private String eerdereOpschortenRedenTekst;

	private boolean geforceerd;
}
