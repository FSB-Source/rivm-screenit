/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

export type ToastActions = ShowToastAction | HideToastAction | HideAllToastsAction

export const SHOW_TOAST = "SHOW_TOAST"
export const HIDE_TOAST = "HIDE_TOAST"
export const HIDE_ALL_TOASTS = "HIDE_ALL_TOASTS"

export type ShowToastAction = { type: "SHOW_TOAST", toast: ToastMessage }

export const createShowToastAction = (toast: ToastMessage): ShowToastAction => ({
	type: SHOW_TOAST,
	toast: toast,
})

export type HideToastAction = { type: "HIDE_TOAST", index: number }

export const createHideToastAction = (index: number): HideToastAction => ({
	type: HIDE_TOAST,
	index: index,
})

export type HideAllToastsAction = { type: "HIDE_ALL_TOASTS" }

export const createHideAllToastsAction = (): HideAllToastsAction => ({
	type: HIDE_ALL_TOASTS,
})
