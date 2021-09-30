/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import type {AnnotatieAfbeelding, AnnotatieAfbeeldingDto} from './AnnotatieAfbeelding';
import {mapAfbeeldingDtoToAfbeelding, mapAfbeeldingToIconenArrayDto} from './AnnotatieAfbeelding';

export type DoorsnedeAfbeeldingen = {
    rechtsVerticaleDoorsnede: ?AnnotatieAfbeelding,
    linksVerticaleDoorsnede: ?AnnotatieAfbeelding,
    rechtsHorizontaleDoorsnede: ?AnnotatieAfbeelding,
    linksHorizontaleDoorsnede: ?AnnotatieAfbeelding,
}

export const newDoorsnedeAfbeeldingen = () => {
    return {
        rechtsVerticaleDoorsnede: null,
        linksVerticaleDoorsnede: null,
        rechtsHorizontaleDoorsnede: null,
        linksHorizontaleDoorsnede: null,
    };
};

export type DoorsnedeAfbeeldingenDto = {
    rechtsVerticaleDoorsnede: AnnotatieAfbeeldingDto,
    linksVerticaleDoorsnede: AnnotatieAfbeeldingDto,
    rechtsHorizontaleDoorsnede: AnnotatieAfbeeldingDto,
    linksHorizontaleDoorsnede: AnnotatieAfbeeldingDto,
}

export const mapDoorsnedeAfbeeldingenFromDto = (doorsnedeAfbeeldingenDto: DoorsnedeAfbeeldingenDto, afspraakId: number): DoorsnedeAfbeeldingen => {
    return {
        linksVerticaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.linksVerticaleDoorsnede),
        rechtsVerticaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.rechtsVerticaleDoorsnede),
        linksHorizontaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.linksHorizontaleDoorsnede),
        rechtsHorizontaleDoorsnede: mapAfbeeldingDtoToAfbeelding(afspraakId, doorsnedeAfbeeldingenDto.rechtsHorizontaleDoorsnede),
    };
};

export const mapDoorsnedeAfbeeldingenToDoorsnedeAfbeeldingenDto = (doorsnedeAfbeeldingen: DoorsnedeAfbeeldingen): DoorsnedeAfbeeldingenDto => {
    return {
        linksHorizontaleDoorsnede: {
            iconen: doorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) : [],
        },
        rechtsHorizontaleDoorsnede: {
            iconen: doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) : [],
        },
        linksVerticaleDoorsnede: {
            iconen: doorsnedeAfbeeldingen.linksVerticaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.linksVerticaleDoorsnede) : [],
        },
        rechtsVerticaleDoorsnede: {
            iconen: doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ? mapAfbeeldingToIconenArrayDto(doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede) : [],
        },
    };
};
