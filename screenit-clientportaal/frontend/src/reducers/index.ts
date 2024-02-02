/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import {combineReducers} from "redux"
import mammaDossierReducer from "./MammaDossierReducer"
import PersoonReducer from "./PersoonReducer"
import LandingOverzichtReducer from "./LandingOverzichtReducer"
import {CLEAR_STATE, RootActions} from "../actions/RootAction"
import {legeState} from "../datatypes/State"
import AuthenticatieReducer from "./AuthenticatieReducer"
import BezwaarReducer from "./BezwaarReducer"
import cervixDossierReducer from "./cervix/CervixDossierReducer"
import colonDossierReducer from "./colon/ColonDossierReducer"
import ToastsReducer from "./ToastsReducer"
import ContactActiesReducer from "./ContactActiesReducer"
import regioReducer from "./RegioReducer"
import EnvironmentInfoReducer from "./EnvironmentInfoReducer"
import SpinnerCounterReducer from "./SpinnerCounterReducer"

const cpReducers = combineReducers({
	authenticatie: AuthenticatieReducer,
	environmentInfo: EnvironmentInfoReducer,
	client: combineReducers({
		beschikbareActies: ContactActiesReducer,
		cervixDossier: cervixDossierReducer,
		colonDossier: colonDossierReducer,
		laatsteBezwaarMoment: BezwaarReducer,
		mammaDossier: mammaDossierReducer,
		persoon: PersoonReducer,
		regio: regioReducer,
	}),
	landingOverzicht: LandingOverzichtReducer,
	toasts: ToastsReducer,
	requestMinusResponseCounter: SpinnerCounterReducer,
})

const rootReducer = (state: any, action: any | RootActions) => {
	if (action.type === CLEAR_STATE) {
		return cpReducers(legeState, action)
	}
	return cpReducers(state, action)
}

export default rootReducer
