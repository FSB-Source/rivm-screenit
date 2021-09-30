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

import type {AnnotatieIcoonType} from './AnnotatieIcoon';

export type IcoonAfbeelding = {
    afbeelding: string,
    width: number,
    height: number,
    isRightUpperCornerOrigin?: boolean
}

export const iconenLijst: Array<AnnotatieIcoonType> = [
    'EENZIJDIGE_BORSTVERKLEINING', 'DUBBELZIJDIGE_BORSTVERKLEINING', 'BORSTVERGROTING', 'UITWENDIGE_AFWIJKING', 'INGETROKKEN_TEPEL',
    'GROTER_DAN', 'KLEINER_DAN', 'AMPUTATIE', 'LITTEKEN_RBLO', 'LITTEKEN_VERTICAAL', 'LITTEKEN_LBRO', 'LITTEKEN_HORIZONTAAL', 'WRAT'];
