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

export type SessionActions = SetSessionAction | ClearSessionAction;

export const SET_SESSION = 'SET_SESSION';
export type SetSessionAction = {
    type: 'SET_SESSION', sessionId: string, gebruikersnaam: string, medewerkercode: string, displayName: string, seCode: string, seNaam: string, yubikeyIdentificatie: string,
    instellingGebruikerId: number
};
export const createActionSetSession = (
    sessionId: string, gebruikersnaam: string, medewerkercode: string, displayName: string, seCode: string, seNaam: string, yubikeyIdentificatie: string,
    instellingGebruikerId: number): SetSessionAction => ({
    type: SET_SESSION,
    sessionId: sessionId,
    gebruikersnaam: gebruikersnaam,
    medewerkercode: medewerkercode,
    displayName: displayName,
    seCode: seCode,
    seNaam: seNaam,
    yubikeyIdentificatie: yubikeyIdentificatie,
    instellingGebruikerId: instellingGebruikerId,
});

export const CLEAR_SESSION = 'CLEAR_SESSION';
export type ClearSessionAction = { type: 'CLEAR_SESSION' };
export const createActionClearSession = (): ClearSessionAction => ({
    type: CLEAR_SESSION,
});
