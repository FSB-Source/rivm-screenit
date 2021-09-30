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

import type {ClientWerklijstItem} from '../datatypes/ClientWerklijstItem';
import {baseUrl, createClientHeaders, fetchApi, fetchApiPromise} from '../util/ApiUtil';
import {showWarningToast, showWijzigingenOpgeslagenToast} from '../util/ToastUtil';
import {store} from '../Store';
import {getMandatory} from '../util/MapUtil';
import {putTransactionToScreenItCentraalPromiseZonderAfspraak} from './TransactionRestclient';
import {createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch, createActionLogGebeurtenisBeeldenGeenMppsOntvangen} from '../actions/LogGebeurtenisActions';
import type {KwaliteitsopnameScreenITWerklijstItem} from '../datatypes/KwaliteitsopnameScreenITWerklijstItem';
import {aeTitle} from '../util/Util';

export const clientToevoegenAanWerklijst = (clientWerklijstItem: ClientWerklijstItem): void => {
    fetchApi('POST', 'werklijst/toevoegen', null, JSON.stringify(clientWerklijstItem));
};

export const kwaliteitsopnameToevoegenAanWerklijst = (kwaliteitsopnameScreenITWerklijstItem: KwaliteitsopnameScreenITWerklijstItem): void => {
    fetchApi('POST', 'werklijst/startKwaliteitsopname', null, JSON.stringify(kwaliteitsopnameScreenITWerklijstItem));
};

export const verwijderVanWerklijst = (aeTitle: string): void => {
    fetchApi('DELETE', 'werklijst/verwijder/' + aeTitle);
};

export const beeindigKwaliteitsopname = (aeTitle: string): void => {
    fetchApi('DELETE', 'werklijst/beeindigKwaliteitsopname/' + aeTitle);
};

export const meldingAfgeslotenProcedure = (clientId: number, uitnodigingsNr: number): void => {
    fetch(baseUrl + 'werklijst/opActieveMppsRecordsLijst/' + uitnodigingsNr, {
        method: 'GET',
        headers: createClientHeaders(),
        body: null,
    }).then(response => {
        if (response.ok) {
            showWarningToast('Controleer of de procedure op de mammograaf is afgesloten. Indien afgesloten, negeer dan deze melding.');
            putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, 'LOG_GEBEURTENIS_SE',
                createActionLogGebeurtenisBeeldenGeenMppsOntvangen()).then(() => {
                showWijzigingenOpgeslagenToast();
            });
        }
    });
};

export const getActieveKwaliteitsopname = (): Promise<KwaliteitsopnameScreenITWerklijstItem> => {
    return fetchApiPromise('GET', 'werklijst/actieveKwaliteitsopname/' + aeTitle()).then(response => {
        return response.json();
    });

};

export const checkOpBeeldenVanAmputatie = (accessionNumber: string, zijde: ?string): Promise<boolean> => {
    zijde = zijde ? zijde : 'GEEN_AMPUTATIE';
    return fetch(baseUrl + 'werklijst/heeftBeeldenZijde/' + accessionNumber + '/' + zijde, {
        method: 'GET',
        headers: createClientHeaders(),
        body: null,
    }).then(response => {
        return response.ok;
    });
};

export const clearWerklijst = () => {
    const aeTitle: ?string = store.getState().huidigeMammograafId ?
        getMandatory(store.getState().mammografenById, store.getState().huidigeMammograafId).aeTitle : null;
    if (aeTitle) {
        verwijderVanWerklijst(aeTitle);
    }
};

export const waarschuwingGecontroleerd = (uitnodigingsNr: number, clientId: number) => {
    putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, 'LOG_GEBEURTENIS_SE',
        createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch()).then(() => {
        showWijzigingenOpgeslagenToast();
    }).then(function() {
        fetch(baseUrl + 'werklijst/waarschuwingGecontroleerd/' + uitnodigingsNr, {
            method: 'POST',
            headers: createClientHeaders(),
            body: null,
        });
    });
};
