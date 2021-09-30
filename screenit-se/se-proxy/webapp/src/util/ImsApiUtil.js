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

import {createEmptyStudyIms, createLogoffIms, createLogonIms, createStudyIms} from './ImsFactory';
import {store} from '../Store';
import {createActionSetStudyForIms} from '../actions/ImsActions';
import {createActionPutIMSConnectieStatus} from '../actions/ConnectieStatusActions';

const url = 'https://localhost:7001';

const getStandardHeaders = (): Headers => {
    const headers = new Headers();
    headers.append('Content-type', 'application/json; charset=utf-8');
    return headers;
};

export const setStudyForIms = (uitnodigingsNr: ?number) => {
    store.dispatch(createActionSetStudyForIms(uitnodigingsNr));
};

export const sendStudyMessageToIMS = (uitnodigingsNr: number, bsn: string, username: string) => {
    setStudyForIms(uitnodigingsNr);
    fetchIMS(createStudyIms(uitnodigingsNr, bsn, username));
};

export const sendEmptyStudyMessageToIMS = (username: string) => {
    setStudyForIms(null);
    fetchIMS(createEmptyStudyIms(username));
};

export const logonToIMS = (username: string): void => {
    setStudyForIms(null);
    fetchIMS(createLogonIms(username));
};

export const logoffToIMS = (username: string) => {
    setStudyForIms(null);
    fetchIMS(createLogoffIms(username));
};

const fetchIMS = (imsAction: any): void => {
    fetch(url, {
        method: 'PUT',
        mode: 'cors',
        headers: getStandardHeaders(),
        body: JSON.stringify(imsAction),
    }).then(() => {
        store.dispatch(createActionPutIMSConnectieStatus('OK'));
    }).catch(() => {
        store.dispatch(createActionPutIMSConnectieStatus('FAULT'));
    });
};
