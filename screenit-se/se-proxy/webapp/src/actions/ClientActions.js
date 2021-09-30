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

import type {Client} from '../datatypes/Client';
import type {TijdelijkAdres} from '../datatypes/TijdelijkAdres';

export type ClientActions = VulClientenAction | SetEmailAdresAction | SetTijdelijkAdresAction | SetTelefoon1Action | SetTelefoon2Action | ClientGegevensOpslaanAction;

export const VUL_CLIENTEN = 'VUL_CLIENTEN';
export type VulClientenAction = { type: 'VUL_CLIENTEN', clienten: Array<Client> }
export const createActionVulClienten = (clienten: Array<Client>): VulClientenAction => ({
    type: VUL_CLIENTEN,
    clienten: clienten,
});

export const SET_EMAILADRES = 'SET_EMAILADRES';
export type SetEmailAdresAction = { type: 'SET_EMAILADRES', clientId: number, emailadres: string }
export const createActionSetEmailAdres = (clientId: number, emailadres: string): SetEmailAdresAction => {
    return {
        type: SET_EMAILADRES,
        clientId: clientId,
        emailadres: emailadres,
    };
};

export const SET_TIJDELIJK_ADRES = 'SET_TIJDELIJK_ADRES';
export type SetTijdelijkAdresAction = { type: 'SET_TIJDELIJK_ADRES', clientId: number, tijdelijkAdres: TijdelijkAdres }
export const createActionSetTijdelijkAdres = (clientId: number, tijdelijkAdres: TijdelijkAdres): SetTijdelijkAdresAction => {
    return {
        type: SET_TIJDELIJK_ADRES,
        clientId: clientId,
        tijdelijkAdres: tijdelijkAdres,
    };
};

export const SET_TELEFOON1 = 'SET_TELEFOON1';
export type SetTelefoon1Action = { type: 'SET_TELEFOON1', clientId: number, telefoon: string }
export const createActionSetTelefoon1 = (clientId: number, telefoon: string): SetTelefoon1Action => {
    return {
        type: SET_TELEFOON1,
        clientId: clientId,
        telefoon: telefoon,
    };
};

export const SET_TELEFOON2 = 'SET_TELEFOON2';
export type SetTelefoon2Action = { type: 'SET_TELEFOON2', clientId: number, telefoon: string }
export const createActionSetTelefoon2 = (clientId: number, telefoon: string): SetTelefoon2Action => {
    return {
        type: SET_TELEFOON2,
        clientId: clientId,
        telefoon: telefoon,
    };
};

type ClientGegevensOpslaanAction = {
    type: 'CLIENTGEGEVENS_OPSLAAN' | 'INSCHRIJVEN',
    afspraakId: number,
    clientId: number,
    tijdelijkAdres: ?TijdelijkAdres,
    telefoonnummer1: ?string,
    telefoonnummer2: ?string,
};
