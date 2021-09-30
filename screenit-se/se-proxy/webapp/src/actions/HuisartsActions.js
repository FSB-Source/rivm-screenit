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

import type {GeenHuisartsOption, Huisarts} from '../datatypes/Huisarts';

export type HuisartsActions = VulHuisartsenByIdAction | KiesHuisartsAction | KiesGeenHuisartsOptieAction;

export const VUL_HUISARTSEN_BY_ID = 'VUL_HUISARTSEN_BY_ID';
export type VulHuisartsenByIdAction = { type: 'VUL_HUISARTSEN_BY_ID', huisartsen: Array<Huisarts> }
export const createActionVulHuisartsenById = (huisartsen: Array<Huisarts>): VulHuisartsenByIdAction => {
    const action = {
        type: VUL_HUISARTSEN_BY_ID,
        huisartsen: huisartsen,
    };
    return action;
};

export const KIES_HUISARTS = 'KIES_HUISARTS';
export type KiesHuisartsAction = { type: 'KIES_HUISARTS', afspraakId: number, huisartsId: number }
export const createActionKiesHuisarts = (afspraakId: number, huisartsId: number): KiesHuisartsAction => {
    const action = {
        type: KIES_HUISARTS,
        afspraakId,
        huisartsId,
    };
    return action;
};

export const KIES_GEEN_HUISARTS_OPTIE = 'KIES_GEEN_HUISARTS_OPTIE';
export type KiesGeenHuisartsOptieAction = { type: 'KIES_GEEN_HUISARTS_OPTIE', afspraakId: number, geenHuisartsOptie: GeenHuisartsOption }
export const createActionKiesGeenHuisartsOptie = (afspraakId: number, geenHuisartsOptie: GeenHuisartsOption): KiesGeenHuisartsOptieAction => {
    const action = {
        type: KIES_GEEN_HUISARTS_OPTIE,
        afspraakId,
        geenHuisartsOptie,
    };
    return action;
};
