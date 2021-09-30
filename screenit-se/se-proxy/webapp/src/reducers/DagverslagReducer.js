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

import type {Dagverslag} from '../datatypes/Dagverslag';
import type {DagverslagActions} from '../actions/DagverslagActions';
import {SET_DAGVERSLAG} from '../actions/DagverslagActions';
import type {ClearCacheActions} from '../actions/ClearCacheActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const dagverslagDefault: Map<string, Dagverslag> = new Map();

const dagverslagReducer = (stateSlice: Map<string, Dagverslag> = dagverslagDefault, action: DagverslagActions | ClearCacheActions): Map<string, Dagverslag> => {
    switch (action.type) {
        case SET_DAGVERSLAG:
            const dagverslag = action.dagverslag;
            stateSlice = stateSlice.set(action.datum, dagverslag);
            return new Map(stateSlice);
        case CLEAR_CACHE:
            return dagverslagDefault;
        default:
            return stateSlice;
    }
};
export default dagverslagReducer;
