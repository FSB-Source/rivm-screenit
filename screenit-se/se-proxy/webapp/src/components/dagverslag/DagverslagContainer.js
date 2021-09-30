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

import {connect} from 'react-redux';
import React from 'react';
import type {State} from '../../datatypes/State';
import DagverslagView from './DagverslagView';
import {createActionShowPopup} from '../../actions/PopupActions';
import AfsprakenDoorvoerenView from './AfsprakenDoorvoerenView';
import {Afspraak} from '../../datatypes/Afspraak';
import {store} from '../../Store';
import {createTransaction, putTransactionsToScreenItCentraalPromise} from '../../restclient/TransactionRestclient';
import {createActionAfspraakDoorvoeren} from '../../actions/AfspraakActions';
import {hasNietDoorgevoerdeOnderzoeken, hasOpenstaandeOnderzoeken} from '../../util/AfsprakenUtil';
import {createActionNavigateToDaglijst} from '../../actions/NavigationActions';
import {dispatchActions} from '../../util/DispatchUtil';
import {showErrorToast, showWijzigingenOpgeslagenToast} from '../../util/ToastUtil';
import {createActionKiesDaglijstDatum} from '../../actions/DaglijstDatumActions';
import {vandaagISO} from '../../util/DateUtil';
import type {SeAction} from '../../actions/SeAction';
import type {Transaction} from '../../datatypes/Transaction';

const mapStateToProps = (state: State) => {
    const voorgaandeDagAfgesloten = store.getState().nietAfgeslotenVanaf === null || state.nietAfgeslotenVanaf === state.daglijstDatum;
    const openstaandeOnderzoeken = hasOpenstaandeOnderzoeken(state.daglijstDatum);
    const nietDoorgevoerdeOnderzoeken = hasNietDoorgevoerdeOnderzoeken(state.daglijstDatum);

    let afsprakenDoorvoerenEnabled = (!openstaandeOnderzoeken && nietDoorgevoerdeOnderzoeken && voorgaandeDagAfgesloten);
    const doorvoerenFeedback = maakFeedback(openstaandeOnderzoeken, nietDoorgevoerdeOnderzoeken, voorgaandeDagAfgesloten);

    return {
        afsprakenDoorvoerenDisabled: !afsprakenDoorvoerenEnabled,
        doorvoerenFeedback: doorvoerenFeedback,
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        showAfsprakenDoorvoerenPopup() {
            if (this.afsprakenDoorvoerenDisabled) {
                showErrorToast(this.doorvoerenFeedback);
            } else {
                dispatch(createActionShowPopup('Alle afgeronde afspraken doorvoeren',
                    <AfsprakenDoorvoerenView/>,
                    () => {
                        afsprakenDoorvoeren();
                    }, () => {
                    },
                    'Akkoord',
                    'Annuleren'));
            }
        },
    };
};

const afsprakenDoorvoeren = () => {
    const huidigeDagAfspraken: Array<Afspraak> = [...store.getState().afsprakenById.values()].filter(
        (afspraak: Afspraak) => afspraak.vanafDatum === store.getState().daglijstDatum,
    );
    let afsprakenDoorvoerenTransactions: Array<Transaction> = [];
    let afsprakenDoorvoerenActions: Array<SeAction> = [];
    huidigeDagAfspraken.forEach(function(afspraak: Afspraak) {
        if (afspraak.status === 'BEEINDIGD' && afspraak.doorgevoerd === false) {
            const afspraakDoorvoerenAction = createActionAfspraakDoorvoeren(afspraak.id);
            afsprakenDoorvoerenActions.push(afspraakDoorvoerenAction);
            afsprakenDoorvoerenTransactions.push(createTransaction(afspraak, 'BEEINDIGDE_AFSPRAAK_DOORVOEREN', afspraakDoorvoerenAction));
        }
    });
    if (afsprakenDoorvoerenActions.length > 0) {
        dispatchActions(store.dispatch, ...afsprakenDoorvoerenActions);
        putTransactionsToScreenItCentraalPromise(afsprakenDoorvoerenTransactions).then(() => {
            showWijzigingenOpgeslagenToast();
            dispatchActions(store.dispatch, createActionKiesDaglijstDatum(vandaagISO()), createActionNavigateToDaglijst());
        });
    }
};

const maakFeedback = (openstaandeOnderzoeken, nietDoorgevoerdeOnderzoeken, voorgaandeDagAfgesloten) => {
    let redenen = [];
    if (!voorgaandeDagAfgesloten) {
        redenen.push('De vorige werkdag is niet correct afgesloten.');
    }
    if (openstaandeOnderzoeken) {
        redenen.push('Er zijn onderzoeken in bewerking.');
    }
    if (!nietDoorgevoerdeOnderzoeken) {
        redenen.push('Geen afgesloten onderzoeken om door te voeren.');
    }
    return redenen.join(' ');
};

const DagverslagContainer = connect(mapStateToProps, mapDispatchToProps)(DagverslagView);

export default DagverslagContainer;
