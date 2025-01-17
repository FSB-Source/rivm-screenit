/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {store} from "../index"

export const loadState = (): any => {
	try {
		const state = sessionStorage.getItem("hpState")
		if (!state) {
			return undefined
		}
		return JSON.parse(state)
	} catch (err) {
		return undefined
	}
}

export const saveState = () => {
	try {
		const state = JSON.stringify(store.getState())
		sessionStorage.setItem("hpState", state)
	} catch {
	}
}

export const removeStateFromStorage = () => {
	try {
		sessionStorage.removeItem("hpState")
	} catch {
	}
}
