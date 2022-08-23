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
import {combineReducers} from "redux"
import {OAuthReducer} from "./OAuthState"
import {UserReducer} from "./UserState"
import {LoadingReducer} from "./LoadingState"
import {LocatieVerificatieReducer} from "./LocatieVerificatieState"
import {BuildInfoReducer} from "./BuildInfoState"
import {ToastsReducer} from "./ToastsState"
import {HuisartsReducer} from "./HuisartsState"
import {LocatiesReducer} from "./LocatiesState"
import {AuthenticationLoadingReducer} from "./AuthenticationLoadingState"

export const CLEAR_STATE = "CLEAR_STATE"
export type ClearStateAction = { type: "CLEAR_STATE" }
export const createClearStateAction = (): ClearStateAction => ({
	type: CLEAR_STATE,
})

const reducers = combineReducers({
	oauth: OAuthReducer,
	user: UserReducer,
	loading: LoadingReducer,
	locatieVerificatie: LocatieVerificatieReducer,
	buildInfo: BuildInfoReducer,
	toasts: ToastsReducer,
	huisarts: HuisartsReducer,
	locaties: LocatiesReducer,
	authenticationLoading: AuthenticationLoadingReducer,
})

export const RootReducer = (state: any, action: any | ClearStateAction) => {
	if (action.type === CLEAR_STATE) {
		return reducers(undefined, action)
	}
	return reducers(state, action)
}

export default RootReducer
