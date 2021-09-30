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

import type {Planning} from '../datatypes/Planning';

export type PlanningActions = VulPlanningAction | ClearPlanningAction;

export const VUL_PLANNING = 'VUL_PLANNING';
export type VulPlanningAction = { type: 'VUL_PLANNING', datum: string, planning: Planning};
export const createActionVulPlanning = (datum: string, planning: Planning): VulPlanningAction => {
    return {
        type: VUL_PLANNING,
        datum: datum,
        planning: planning,
    };
};

export const CLEAR_PLANNING = 'CLEAR_PLANNING';
export type ClearPlanningAction = { type: 'CLEAR_PLANNING', datum: string};
export const createActionClearPlanning = (datum: string): ClearPlanningAction => {
    return {
        type: CLEAR_PLANNING,
        datum: datum
    }
};
