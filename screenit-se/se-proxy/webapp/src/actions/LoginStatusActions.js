/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

export type LoginStatusActions = LoginActiveAction | LoginInactiveAction | StopIdentificerenTotYubikeyErafAction | ClearStopIdentificerenTotYubikeyErafAction;

export const SET_LOGIN_ACTIVE = 'SET_LOGIN_ACTIVE';
export type LoginActiveAction = { type: 'SET_LOGIN_ACTIVE' };
export const createActionLoginActive = (): LoginActiveAction => ({type: SET_LOGIN_ACTIVE});

export const SET_LOGIN_INACTIVE = 'SET_LOGIN_INACTIVE';
export type LoginInactiveAction = { type: 'SET_LOGIN_INACTIVE' };
export const createActionLoginInactive = (): LoginInactiveAction => ({type: SET_LOGIN_INACTIVE});

export const SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF = 'SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF';
export type StopIdentificerenTotYubikeyErafAction = { type: 'SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF' };
export const createActionStopIdentificerenTotYubikeyEraf = (): StopIdentificerenTotYubikeyErafAction => ({type: SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF});

export const CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF = 'CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF';
export type ClearStopIdentificerenTotYubikeyErafAction = { type: 'CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF' };
export const createActionClearStopIdentificerenTotYubikeyEraf = (): ClearStopIdentificerenTotYubikeyErafAction => ({type: CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF});
