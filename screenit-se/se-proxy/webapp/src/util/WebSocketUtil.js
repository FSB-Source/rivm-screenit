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
import {vernieuwAfsprakenDaglijst} from '../restclient/DaglijstRestclient';
import {createActionQueueDaglijstVerversen} from '../actions/UpdateAction';
import type {Transaction} from '../datatypes/Transaction';
import {dispatchActions} from './DispatchUtil';
import Stomp from '../../node_modules/stompjs';
import {createActionOffline, createActionOnline} from '../actions/ConnectionActions';
import {persistentErrorToast, persistentSuccessToast} from './ToastUtil';
import {nuISO, setOffset} from './DateUtil';
import * as moment from 'moment';
import {Duration} from 'moment';
import {fetchApiPromise} from './ApiUtil';

let stompClient = null;

export const initWebSocket = () => {
    stompClient = Stomp.client(((window.location.protocol === 'https:') ? 'wss://' : 'ws://') + window.location.host + '/transactionBroadcaster');
    stompClient.debug = null;
    stompClient.connect({},
        () => {
            console.log('Websocket aanmelden bij proxy endpoint.');
            stompClient.subscribe('/transactionReceive', function(transaction: string) {
                verwerkSocketBericht(transaction);
            });
        }, () => {
            window.setTimeout(() => {
                console.log('Probeer opnieuw een socket verbinding te maken met de proxy...');
                initWebSocket();
            }, 1000);
        });
    forceerUpdateVoorVerkeerdeVersie();
};

const forceerUpdateVoorVerkeerdeVersie = () => {
    const prevVersion = store.getState().environmentInfo.version;
    fetchApiPromise('GET', 'environmentInfo').then((response) => {
        response.json().then(function(environmentInfo) {
            if (environmentInfo.version && prevVersion && environmentInfo.version !== prevVersion) {
                window.onbeforeunload = function(): boolean {
                    return undefined; 
                };
                window.location.reload();
                persistentSuccessToast('De SE is geüpdatet naar de nieuwste versie.');
            }
        });
    });
};

const DAGLIJST_UPDATE = 'DAGLIJST_UPDATE';
const TIJD_UPDATE = 'TIJD_UPDATE';
const ONLINE = 'ONLINE';
const OFFLINE = 'OFFLINE';
const SERVER_ERROR = 'SERVER_ERROR';
const verwerkSocketBericht = (text: string) => {
    const message: string = text.body;
    const command = message.split('###')[0];
    switch (command) {
        case ONLINE:
            dispatchActions(store.dispatch, createActionOnline());
            break;
        case OFFLINE:
            dispatchActions(store.dispatch, createActionOffline());
            break;
        case DAGLIJST_UPDATE:
            updateDaglijst();
            break;
        case TIJD_UPDATE:
            const duration: string = message.split('###')[1];
            const momentDuration: Duration = moment.duration(duration);
            setOffset(momentDuration);
            console.log('De tijd is verzet naar: ' + nuISO());
            break;
        case SERVER_ERROR:
            persistentErrorToast(message.split('###')[1]);
            break;
        default:
            const receivedTransaction: Transaction = JSON.parse(message);
            dispatchActions(store.dispatch, ...receivedTransaction.actions);
    }
};

const updateDaglijst = () => {
    if (store.getState().navigation.tab === 'Daglijst') {
        if (store.getState().formsByFormId.get('passant_afspraak_maken') && store.getState().formsByFormId.get('passant_afspraak_maken').isSubmitted) {
        } else {
            vernieuwAfsprakenDaglijst();
        }
    } else {
        store.dispatch(createActionQueueDaglijstVerversen());
    }
};

function disconnect() {
    if (stompClient !== null) {
        stompClient.disconnect();
    }
}
