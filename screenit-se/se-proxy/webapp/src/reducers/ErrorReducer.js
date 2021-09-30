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

import type {ErrorActions} from '../actions/ErrorActions';
import {FATAL_ERROR} from '../actions/ErrorActions';
import type {ErrorDto} from '../datatypes/ErrorDto';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const errorReducer = (stateSlice: ErrorDto | null = null, action: ErrorActions): ErrorDto | null => {
    switch (action.type) {
        case FATAL_ERROR:
            return action.error;
        case CLEAR_CACHE:
            return null;
        default:
            return stateSlice;
    }
};

export default errorReducer;
