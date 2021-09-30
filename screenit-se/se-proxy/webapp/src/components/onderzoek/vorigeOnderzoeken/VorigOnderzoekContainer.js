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

import type {State} from '../../../datatypes/State';
import type {VorigOnderzoekProps} from './VorigOnderzoekView';
import VorigOnderzoekView from './VorigOnderzoekView';
import {connect} from 'react-redux';
import {putTransactionToScreenItCentraalPromiseZonderAfspraak} from '../../../restclient/TransactionRestclient';
import {sendStudyMessageToIMS} from '../../../util/ImsApiUtil';
import {createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald} from '../../../actions/LogGebeurtenisActions';
import {showWijzigingenOpgeslagenToast} from '../../../util/ToastUtil';

const mapStateToProps = (state: State, ownProps: VorigOnderzoekProps) => {
    return ownProps;
};

const mapDispatchToProps = () => {
    return {
        vorigeOnderzoekOphalen(clientId: number, uitnodigingsNr: number, username: string, bsn: string): void {
            sendStudyMessageToIMS(uitnodigingsNr, bsn, username);
            putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, 'LOG_GEBEURTENIS_SE',
                createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald()).then(() => {
                showWijzigingenOpgeslagenToast();
            });
        },
    };
};

const VorigOnderzoekContainer = connect(mapStateToProps, mapDispatchToProps)(VorigOnderzoekView);

export default VorigOnderzoekContainer;
