/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {ToastMessage} from "../datatypes/toast/ToastMessage"
import {ToastActions} from "../actions/ToastAction"

function ToastsReducer(stateSlice: ToastMessage[] = [], action: ToastActions): ToastMessage[] {
	const toasts = stateSlice.slice()
	switch (action.type) {
		case "SHOW_TOAST":
			const {type, description, title} = action.toast
			const toastZitNogNietInLijst = toasts.findIndex(value => value.type === type && value.description === description && value.title === title) === -1
			if (toastZitNogNietInLijst) {
				toasts.push(action.toast)
			}
            return toasts;
        case "HIDE_TOAST":
            toasts.splice(action.index, 1)
            return toasts;
        case "HIDE_ALL_TOASTS":
            return [];
        default:
            return stateSlice;
    }
}

export default ToastsReducer;
