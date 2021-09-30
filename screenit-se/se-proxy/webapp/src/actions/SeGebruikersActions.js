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

export type SeGebruikersActions = AddAllSeGebruikersAction | AddSeGebruikerAction | ClearSeGebruikersAction;

export const ADD_ALL_SE_GEBRUIKERS = 'ADD_ALL_SE_GEBRUIKERS';
export type AddAllSeGebruikersAction = { type: 'ADD_ALL_SE_GEBRUIKERS', seGebruikers: Map<number, string>};
export const createActionAddAllSeGebruikers = (seGebruikers: Map<number, string>): AddAllSeGebruikersAction => {
    return {
        type: ADD_ALL_SE_GEBRUIKERS,
        seGebruikers: seGebruikers,
    };
};

export const ADD_SE_GEBRUIKER = 'ADD_SE_GEBRUIKER';
export type AddSeGebruikerAction = { type: 'ADD_SE_GEBRUIKER', instellingGebruikerId: string, displayName: string };
export const createActionAddSeGebruiker = (instellingGebruikerId: string, displayName: string): AddSeGebruikerAction => {
    return {
        type: ADD_SE_GEBRUIKER,
        instellingGebruikerId: instellingGebruikerId,
        displayName: displayName,
    };
};

export const CLEAR_SE_GEBRUIKERS = 'CLEAR_SE_GEBRUIKERS';
export type ClearSeGebruikersAction = { type: 'CLEAR_SE_GEBRUIKERS'};
export const createActionClearSeGebruikers = (): ClearSeGebruikersAction => {
    return {
        type: CLEAR_SE_GEBRUIKERS,
    };
};
