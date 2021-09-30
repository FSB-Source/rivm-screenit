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
import type {VulMammografenAction} from '../actions/MammograafActions';
import {VUL_MAMMOGRAFEN} from '../actions/MammograafActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const mammografenReducer = (stateSlice: Map<number, Mammograaf> = new Map(), action: VulMammografenAction): Map<number, Mammograaf> => {
    switch (action.type) {
        case VUL_MAMMOGRAFEN:
            return action.mammografen.reduce((result: Map<number, Mammograaf>, mammograaf: Mammograaf) => {
                return result.set(mammograaf.id, mammograaf);
            }, new Map());
        case CLEAR_CACHE:
            return new Map();
        default:
            return stateSlice;
    }
};

export default mammografenReducer;
