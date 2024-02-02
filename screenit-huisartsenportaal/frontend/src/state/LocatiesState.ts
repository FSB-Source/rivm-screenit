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
import {LocatieDto, LocatieStatus} from "./datatypes/dto/LocatieDto"

export interface LocatiesResponse {
	aantalLocaties: number;
	locaties: LocatieDto[]
}

export interface LocatiesState {
	values: LocatiesResponse;
	filter: LocatieStatus
}

const LocatiesStateSlice = createSlice({
	name: "locaties",
	initialState: {
		values: {
			aantalLocaties: 0,
			locaties: [],
		},
		filter: LocatieStatus.ACTIEF,
	} as LocatiesState,
	reducers: {
		set: (state: LocatiesState, {payload: locaties}: PayloadAction<LocatiesState>) => {
			return locaties
		},
	},
})

export const LocatiesReducer = LocatiesStateSlice.reducer
export const {set: createActionSetLocaties} = LocatiesStateSlice.actions
