/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {createSlice, PayloadAction} from "@reduxjs/toolkit"
import {Toast, ToastType} from "./datatypes/Toast"

const ToastsStateSlice = createSlice({
	name: "toasts",
	initialState: [] as Toast[],
	reducers: {
		push: (state: Toast[], {payload: toast}: PayloadAction<{ type: ToastType, message: string }>) => {
			if (state.filter((t) => t.message === toast.message).length > 0) {
				return [...state]
			}
			return [...state, {
				type: toast.type,
				message: toast.message,
				id: new Date().getTime(),
			}]
		},
		hide: (state: Toast[], {payload: toastId}: PayloadAction<number>) => {
			return [...state.filter((toast) => toast.id !== toastId)]
		},
		clear: () => {
			return []
		},
	},
})

export const ToastsReducer = ToastsStateSlice.reducer
export const {push: createActionPushToast, hide: createActionHideToast, clear: createActionClearToasts} = ToastsStateSlice.actions
