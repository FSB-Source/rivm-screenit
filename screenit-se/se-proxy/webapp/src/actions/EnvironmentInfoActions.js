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

import type {EnvironmentInfo} from '../datatypes/EnvironmentInfo';
import {AANTAL_INTERVALLEN_VOOR_LOGOUT, startPollingNfc} from '../util/NfcUtil';

export type EnvironmentInfoActions = SetEnvironmentInfoAction;

export const SET_ENVIRONMENTINFO = 'SET_ENVIRONMENTINFO';
export type SetEnvironmentInfoAction = { type: 'SET_ENVIRONMENTINFO', environment: EnvironmentInfo }
export const createActionSetEnvironmentInfo = (environmentInfo: EnvironmentInfo): SetEnvironmentInfoAction => {
    let action = {
        type: SET_ENVIRONMENTINFO,
        environment: environmentInfo,
    };
    if (environmentInfo.nfcEnabled) {
        startPollingNfc(AANTAL_INTERVALLEN_VOOR_LOGOUT);
    }
    return action;
};
