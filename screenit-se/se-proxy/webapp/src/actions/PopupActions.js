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

import React from 'react';

export type PopupActions = ShowPopupAction | AkkoordPopupAction | ClearPopupAction;

export const SHOW_POPUP = 'SHOW_POPUP';
export type ShowPopupAction = { type: 'SHOW_POPUP', titel: string, body: Object, callback: () => void, cancelCallback: ?() => void, akkoordString: ?string, annulerenString: ?string };
export const createActionShowPopup = (
    titel: string, body: Object, callback: () => void, cancelCallback: ?() => void, akkoordString: ?string, annulerenString: ?string): ShowPopupAction => {
    return {
        type: SHOW_POPUP,
        titel: titel,
        body: body,
        callback: callback,
        cancelCallback: cancelCallback,
        akkoordString: akkoordString,
        annulerenString: annulerenString,
    };
};

export const AKKOORD_POPUP = 'AKKOORD_POPUP';
export type AkkoordPopupAction = { type: 'AKKOORD_POPUP' };
export const createActionAkkoordPopup = (): AkkoordPopupAction => {
    return {
        type: AKKOORD_POPUP,
    };
};
export const CLEAR_POPUP = 'CLEAR_POPUP';
export type ClearPopupAction = { type: 'CLEAR_POPUP' };
export const createActionClearPopup = (): ClearPopupAction => {
    return {
        type: CLEAR_POPUP,
    };
};
