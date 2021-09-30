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

import type {AnnotatieIcoon, AnnotatieIcoonDto} from './AnnotatieIcoon';

let nextUnusedIcoonId: number = 1;

export const getNextIcoonId = (): number => {
    return nextUnusedIcoonId++;
};

export type AnnotatieAfbeeldingDto = {
    id?: number,
    iconen: Array<AnnotatieIcoonDto>
}

export type AnnotatieAfbeelding = {
    afspraakId: number,
    iconenById: Map<number, AnnotatieIcoon>
}

export const mapAfbeeldingToIconenArrayDto = (afbeelding: AnnotatieAfbeelding): Array<AnnotatieIcoonDto> => {
    if (afbeelding.iconenById === null) {
        return [];
    }
    return Array.from(afbeelding.iconenById.values()).map((value) => {
        return {
            positieX: value.positieX,
            positieY: value.positieY,
            tekst: value.tekst,
            type: value.type,
        };
    });
};

export const mapAfbeeldingToEnkelDto = (afspraakId: number, afbeelding: AnnotatieAfbeelding): AnnotatieAfbeeldingDto => {
    const afbeeldingDto: AnnotatieAfbeeldingDto = {id: afspraakId, iconen: mapAfbeeldingToIconenArrayDto(afbeelding)};
    return afbeeldingDto;
};

export const mapAfbeeldingDtoToAfbeelding = (afspraakId: number, afbeeldingDto: ?AnnotatieAfbeeldingDto): AnnotatieAfbeelding => {
    if (!afbeeldingDto || !afbeeldingDto.iconen) {
        return {afspraakId: afspraakId, iconenById: new Map()};
    }
    return {
        afspraakId: afspraakId,
        iconenById: new Map(afbeeldingDto.iconen.map((i) => {
            const id = getNextIcoonId();
            return [id, {icoonId: id, positieX: i.positieX, positieY: i.positieY, tekst: i.tekst, type: i.type}];
        })),
    };
};
