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

import type {Zorginstelling} from '../datatypes/Zorginstelling';
import type {ZorginstellingActions} from '../actions/ZorginstellingActions';
import {VUL_ZORGINSTELLINGEN} from '../actions/ZorginstellingActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const zorginstellingenReducer = (stateSlice: Map<number, Zorginstelling> = new Map(), action: ZorginstellingActions): Map<number, Zorginstelling> => {
    const result: Map<number, Zorginstelling> = new Map();
    switch (action.type) {
        case VUL_ZORGINSTELLINGEN:
            action.zorginstellingen.map((z: Zorginstelling) => result.set(z.id, z));
            break;
        case CLEAR_CACHE:
            return new Map();
        default:
            return stateSlice;
    }
    return new Map([...stateSlice, ...result]);
};

export default zorginstellingenReducer;
