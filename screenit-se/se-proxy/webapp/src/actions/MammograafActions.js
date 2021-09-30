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

import {Mammograaf} from '../datatypes/Mammograaf';

export type MammograafActions = VulMammografenAction | SetHuidigeMammograafAction;

export const VUL_MAMMOGRAFEN = 'VUL_MAMMOGRAFEN';
export type VulMammografenAction = { type: 'VUL_MAMMOGRAFEN', mammografen: Array<Mammograaf> }
export const createActionVulMammografen = (mammografen: Array<Mammograaf>): VulMammografenAction => {
    return {
        type: VUL_MAMMOGRAFEN,
        mammografen,
    };
};

export const SET_HUIDIGE_MAMMOGRAAF = 'SET_HUIDIGE_MAMMOGRAAF';
export type SetHuidigeMammograafAction = { type: 'SET_HUIDIGE_MAMMOGRAAF', mammograafId: number }
export const createActionSetHuidigeMammograaf = (mammograafId: number): SetHuidigeMammograafAction => {
    return {
        type: SET_HUIDIGE_MAMMOGRAAF,
        mammograafId,
    };
};
