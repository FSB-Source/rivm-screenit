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

import type {LoginStatusActions} from '../actions/LoginStatusActions';
import {CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF, SET_LOGIN_ACTIVE, SET_LOGIN_INACTIVE, SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF} from '../actions/LoginStatusActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';
import type {LoginStatus} from '../datatypes/LoginStatus';

const initialLoginStatus: LoginStatus = {
    inlogActief: false,
    stopIdentificerenTotYubikeyEraf: false,
};

const loginStatusReducer = (stateSlice: LoginStatus = initialLoginStatus, action: LoginStatusActions): LoginStatus => {
    switch (action.type) {
        case SET_LOGIN_ACTIVE:
            return {...stateSlice, inlogActief: true};
        case SET_LOGIN_INACTIVE:
            return {...stateSlice, inlogActief: false};
        case SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF:
            return {...stateSlice, stopIdentificerenTotYubikeyEraf: true};
        case CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF:
            return {...stateSlice, stopIdentificerenTotYubikeyEraf: false};
        case CLEAR_CACHE:
            return initialLoginStatus;
        default:
            return stateSlice;
    }
};

export default loginStatusReducer;
