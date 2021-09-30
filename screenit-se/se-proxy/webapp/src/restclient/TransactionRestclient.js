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

import {fetchApiPromise} from '../util/ApiUtil';
import type {Transaction, TransactionType} from '../datatypes/Transaction';
import {store} from '../Store';
import type {SeAction} from '../actions/SeAction';
import {Afspraak} from '../datatypes/Afspraak';

export const putTransactionToScreenItCentraalPromiseZonderAfspraak = (
    clientId: number, uitnodigingsNr: ?number, type: TransactionType, ...actions: Array<SeAction>): Promise<any> => {
    const transaction: Transaction = {
        type: type,
        clientId: clientId,
        sessionId: store.getState().session.sessionId,
        medewerkercode: store.getState().session.medewerkercode,
        instellingGebruikerId: store.getState().session.instellingGebruikerId,
        afspraakVanafDatum: store.getState().daglijstDatum,
        uitnodigingsNr: uitnodigingsNr,
        actions: actions,
    };
    return promisePutTransactionToCentraal(transaction);
};

const createTransactionWithActionArray = (afspraak: ?Afspraak, type: TransactionType, actions: Array<SeAction>): Transaction => {
    return {
        type: type,
        clientId: afspraak ? afspraak.clientId : null,
        sessionId: store.getState().session.sessionId,
        medewerkercode: store.getState().session.medewerkercode,
        instellingGebruikerId: store.getState().session.instellingGebruikerId,
        afspraakVanafDatum: afspraak ? afspraak.vanafDatum : store.getState().daglijstDatum,
        uitnodigingsNr: afspraak ? afspraak.uitnodigingsNr : null,
        actions: actions,
    };
};

export const createTransaction = (afspraak: ?Afspraak, type: TransactionType, ...actions: Array<SeAction>): Transaction => {
    return createTransactionWithActionArray(afspraak, type, actions);
};

export const putTransactionsToScreenItCentraalPromise = (transactions: Array<Transaction>): Promise<any> => {
    return promisePutTransactionToCentraal(transactions);
};

export const putTransactionToScreenItCentraalPromise = (afspraak: ?Afspraak, type: TransactionType, ...actions: Array<SeAction>): Promise<any> => {
    const transaction = createTransactionWithActionArray(afspraak, type, actions);
    return promisePutTransactionToCentraal(transaction);
};

export const promisePutTransactionToCentraal = (transaction: Transaction | Array<Transaction>): Promise<any> => {
    return new Promise(function(resolve) {
        transactionQueue.unshift({
            transaction: transaction,
            transactionResolve: resolve,
        });

        if (!verwerkingInProgress) {
            verwerkQueue();
        }
    });
};

let verwerkingInProgress = false;

const transactionQueue = [];

const verwerkQueue = () => {
    verwerkingInProgress = true;
    if (transactionQueue.length === 0) {
        verwerkingInProgress = false;
        return;
    }
    const transactionObject = transactionQueue.pop();
    fetchApiPromise('PUT', 'putTransactionToScreenItCentraal', JSON.stringify(transactionObject.transaction)).then(function() {
        transactionObject.transactionResolve();
        verwerkQueue();
    });
};
