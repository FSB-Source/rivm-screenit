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
import {AuthenticatieAction, SET_LOGGED_IN, SET_LOGGING_IN, SET_LOGGING_OUT, SET_SESSION_EXPIRED, SET_UNAUTHORIZED} from "../actions/AuthenticatieAction"
import {AuthenticatieState, legeAuthenticatieState} from "../datatypes/authenticatie/AuthenticatieState"

function AuthenticatieReducer(stateSlice: AuthenticatieState = legeAuthenticatieState, action: AuthenticatieAction): AuthenticatieState {
	switch (action.type) {
		case SET_LOGGING_IN:
			return {
				...stateSlice,
				isLoggingIn: action.loggingIn,
				isLoggingOut: action.loggingIn ? false : stateSlice.isLoggingOut,
			}
		case SET_LOGGING_OUT:
			return {
				...stateSlice,
				isLoggingIn: action.loggingOut ? false : stateSlice.isLoggingIn,
				isLoggingOut: action.loggingOut,
				isSessionExpired: action.loggingOut ? false : stateSlice.isSessionExpired,
			}
		case SET_LOGGED_IN:
			return {
				...stateSlice,
				isLoggingIn: action.loggedIn ? false : stateSlice.isLoggingIn,
				isLoggedIn: action.loggedIn,
				isLoggingOut: action.loggedIn ? false : stateSlice.isLoggingOut,
				isSessionExpired: action.loggedIn ? false : stateSlice.isSessionExpired,
			}
		case SET_SESSION_EXPIRED:
			return {
				...stateSlice,
				isSessionExpired: action.sessionExpired,
			}
		case SET_UNAUTHORIZED:
			return {
				...stateSlice,
				isUnauthorized: action.unauthorized,
			}
		default:
			return stateSlice
	}
}

export default AuthenticatieReducer
