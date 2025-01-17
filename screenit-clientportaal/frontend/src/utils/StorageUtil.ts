/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {legeState, State} from "../datatypes/State"
import {cpStore} from "../index"
import {transformDates} from "./DateTransformUtil"

export const loadState = (): State => {
	try {
		const cpState = sessionStorage.getItem("cpState")
		if (!cpState) {
			return legeState
		}
		const parsedState = transformDates(JSON.parse(cpState))
		return {
			...parsedState,
			requestMinusResponseCounter: 0,
		}
	} catch (err) {
		return legeState
	}
};

export const saveState = () => {
    try {
        const cpState = JSON.stringify(cpStore.getState());
        sessionStorage.setItem('cpState', cpState);
    } catch {
    }
};

export const removeStateFromStorage = () => {
    try{
        sessionStorage.removeItem("cpState")
    } catch {
    }
}
