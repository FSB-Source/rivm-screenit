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

import type {PlanningActions} from '../actions/PlanningActions';
import {CLEAR_PLANNING, VUL_PLANNING} from '../actions/PlanningActions';
import type {Planning} from '../datatypes/Planning';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

type PlanningMap = Map<string, Planning>;

const planningReducer = (stateSlice: PlanningMap = new Map(), action: PlanningActions): PlanningMap => {
    const result: Map<string, Planning> = new Map();

    if (action.type === VUL_PLANNING) {
        const planning: Planning = action.planning;
        result.set(action.datum, planning);
        return new Map([...stateSlice, ...result]);
    } else if (action.type === CLEAR_PLANNING) {
        stateSlice.delete(action.datum);
        return stateSlice;
    } else if (action.type === CLEAR_CACHE) {
        return new Map();
    }
    return stateSlice;
};

export default planningReducer;
