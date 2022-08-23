/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {createSlice, PayloadAction} from "@reduxjs/toolkit"
import {HuisartsDto} from "./datatypes/dto/HuisartsDto"

const HuisartsStateSlice = createSlice({
	name: "huisarts",
	initialState: null as HuisartsDto | null,
	reducers: {
		set: (state: HuisartsDto | null, {payload: huisarts}: PayloadAction<HuisartsDto>) => {
			return huisarts
		},
		clear: () => {
			return null
		},
	},
})

export const HuisartsReducer = HuisartsStateSlice.reducer
export const {set: createActionSetHuisarts, clear: createActionClearHuisarts} = HuisartsStateSlice.actions
