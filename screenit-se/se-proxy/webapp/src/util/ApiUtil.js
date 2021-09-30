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

import {store} from '../Store';
import {getTokenHeader} from './TokenUtil';
import {createActionFatalError} from '../actions/ErrorActions';
import {MELDING_SESSIE_NIET_GELDIG, MELDING_TECHNISCHE_FOUT, showErrorToast, showErrorToastWithoutAutoClose} from './ToastUtil';
import {logoutClient} from '../restclient/AuthenticatieRestclient';
import type {ErrorDto} from '../datatypes/ErrorDto';
import {datumFormaat, nu} from './DateUtil';

export const baseUrl = './api/';

export const fetchApi = (method: string, url: string, callBack: ?Function, body: ? string) => {
    fetch(baseUrl + url,
        {
            method: method,
            headers: createClientHeaders(),
            body: body,
        }).then((response: any) => {
        if (response.status === 403) {
            console.log('Uitloggen door HTTP status 403: ' + (getYubikeyIdentificatie() || 'onbekend'));
            logoutClient();
        } else if (!response.ok) {
            if (!response.data) {
                console.warn('Geen response data bij api call met status ' + response.status + ': ' + baseUrl + url + ':' + method);
                const errorVanSe: ErrorDto = {errorReferentie: store.getState().session.seCode + '-' + datumFormaat(nu())};
                store.dispatch(createActionFatalError(errorVanSe));
            } else {
                response.json().then(error => {
                    if (error && error.errorReferentie) {
                        console.warn(error.errorReferentie);
                        store.dispatch(createActionFatalError(error));
                    } else {

                        const errorVanSe: ErrorDto = {errorReferentie: store.getState().session.seCode + '-' + datumFormaat(nu())};
                        console.warn(errorVanSe);
                        store.dispatch(createActionFatalError(errorVanSe));
                    }
                });
            }
        } else if (callBack) {
            return response.json().then(result => {
                if (callBack) {
                    try {
                        callBack(result);
                    } catch (error) {
                        showErrorToast(MELDING_TECHNISCHE_FOUT);
                        console.log('Error: ' + error);
                    }
                }
            });
        }
    }).catch((errorResponse) => {
            console.log(errorResponse.toString());
        },
    );
};

export const fetchApiPromise = (method: string, url: string, body: ? string): Promise<any> => {
    return new Promise(function(resolve, reject) {
        fetch(baseUrl + url,
            {
                method: method,
                headers: createClientHeaders(),
                body: body,
            },
        ).then(response => {
                if (response.status === 403) {
                    console.log('Uitloggen door HTTP status 403: ' + (getYubikeyIdentificatie() || 'onbekend'));
                    logoutClient();
                    showErrorToastWithoutAutoClose(MELDING_SESSIE_NIET_GELDIG);
                    reject('Not authorized');
                } else if (response.status === 500) {
                    response.json().then(error => {
                        console.log(error.errorReferentie);
                        store.dispatch(createActionFatalError(error));
                    }).catch(error => {
                        showErrorToast(MELDING_TECHNISCHE_FOUT);
                        console.log('Error response: ' + error);
                    });
                    reject(response);
                } else {
                    return resolve(response);
                }
            },
        ).catch(errorResponse => {
            reject(errorResponse);
        });
    });
};

function getYubikeyIdentificatie() {
    return store.getState().session ? store.getState().session.yubikeyIdentificatie : null;
}

function getAccountId() {
    return store.getState().session ? store.getState().session.instellingGebruikerId : null;
}

export const createClientHeaders = () => {
    let clientHeaders = getTokenHeader();
    clientHeaders.append('Accept', 'application/json, text/plain, */*');
    clientHeaders.append('Content-Type', 'application/json');
    clientHeaders.append('YubikeyIdentificatie', getYubikeyIdentificatie());
    clientHeaders.append('accountId', getAccountId());
    return clientHeaders;
};
