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

import type {BiradsWaarde} from './BiradsWaarde';
import type {DoorsnedeAfbeeldingen, DoorsnedeAfbeeldingenDto} from './DoorsnedeAfbeeldingen';
import {mapDoorsnedeAfbeeldingenFromDto} from './DoorsnedeAfbeeldingen';
import type {VorigOnderzoek} from './VorigOnderzoek';

export type LezingDto = {
    radioloogNaam: string;
    lezingType: string;
    biradsRechts: BiradsWaarde;
    biradsLinks: BiradsWaarde;
    lezingAanzichten: DoorsnedeAfbeeldingenDto
}

export type Lezing = {
    radioloogNaam: string;
    lezingType: string;
    biradsRechts: BiradsWaarde;
    biradsLinks: BiradsWaarde;
    lezingAanzichten: DoorsnedeAfbeeldingen | null,
    vorigOnderzoek: VorigOnderzoek,
}

export function mapLezingDtoToLezing(lezingDto: LezingDto, vorigOnderzoek: VorigOnderzoek): Lezing {
    return {
        radioloogNaam: lezingDto.radioloogNaam,
        lezingType: lezingDto.lezingType,
        biradsRechts: lezingDto.biradsRechts,
        biradsLinks: lezingDto.biradsLinks,
        lezingAanzichten: lezingDto.lezingAanzichten ? mapDoorsnedeAfbeeldingenFromDto(lezingDto.lezingAanzichten, 0) : null,
        vorigOnderzoek,
    };
}
