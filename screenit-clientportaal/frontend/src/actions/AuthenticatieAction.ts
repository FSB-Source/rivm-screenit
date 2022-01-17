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
export type AuthenticatieAction =
    SetLoggingInAction
    | SetLoggingOutAction
    | SetLoggedInAction
    | SetSessionExpiredAction

export const SET_LOGGING_IN = 'SET_LOGGING_IN';

export type SetLoggingInAction = { type: 'SET_LOGGING_IN', loggingIn: boolean }

export const setLoggingInAction = (loggingIn: boolean): SetLoggingInAction => ({
    type: SET_LOGGING_IN,
    loggingIn: loggingIn
})

export const SET_LOGGING_OUT = 'SET_LOGGING_OUT';

export type SetLoggingOutAction = { type: 'SET_LOGGING_OUT', loggingOut: boolean }

export const setLoggingOutAction = (loggingOut: boolean): SetLoggingOutAction => ({
    type: SET_LOGGING_OUT,
    loggingOut: loggingOut
})

export const SET_LOGGED_IN = 'SET_LOGGED_IN';

export type SetLoggedInAction = { type: 'SET_LOGGED_IN', loggedIn: boolean }

export const setLoggedInAction = (loggedIn: boolean): SetLoggedInAction => ({
    type: SET_LOGGED_IN,
    loggedIn: loggedIn
})

export const SET_SESSION_EXPIRED = 'SET_SESSION_EXPIRED';

export type SetSessionExpiredAction = { type: 'SET_SESSION_EXPIRED', sessionExpired: boolean }

export const setSessionExpiredAction = (sessionExpired: boolean): SetSessionExpiredAction => ({
    type: SET_SESSION_EXPIRED,
    sessionExpired: sessionExpired,
})
