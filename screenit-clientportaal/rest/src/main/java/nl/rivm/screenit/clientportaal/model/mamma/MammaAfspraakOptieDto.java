package nl.rivm.screenit.clientportaal.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.clientportaal.model.ClientportaalBaseDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;

@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class MammaAfspraakOptieDto extends ClientportaalBaseDto
{
    private Long capaciteitBlokId;

    private LocalDateTime datumTijd;

    private Long standplaatsPeriodeId;

    private Double afstand;

    private String adres;

    private String postcode;

    private String plaats;

    private boolean bevestigingsBrief;

	private boolean toonBevestigingsBriefOptie;

    private MammaAfspraakZoekFilterDto filter;

    public MammaAfspraakOptieDto(MammaKandidaatAfspraakDto kandidaatAfspraakDto)
    {
        this.capaciteitBlokId = kandidaatAfspraakDto.getCapaciteitBlokId();
        this.datumTijd = kandidaatAfspraakDto.getTijd().atDate(kandidaatAfspraakDto.getDatum());
        this.standplaatsPeriodeId = kandidaatAfspraakDto.getStandplaatsPeriodeId();
        this.afstand = kandidaatAfspraakDto.getAfstand();
    }
}
