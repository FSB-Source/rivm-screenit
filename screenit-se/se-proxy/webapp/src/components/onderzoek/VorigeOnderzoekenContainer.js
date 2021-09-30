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
import type {VorigeOnderzoekenConfirmBtnKind} from './VorigeOnderzoekenView';
import VorigeOnderzoekenView from './VorigeOnderzoekenView';
import {createActionNavigateToOnderzoek} from '../../actions/NavigationActions';
import {createActionOnderzoekStarten} from '../../actions/OnderzoekActions';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Client} from '../../datatypes/Client';
import type {State} from '../../datatypes/State';
import {putTransactionToScreenItCentraalPromise} from '../../restclient/TransactionRestclient';
import {dispatchActions} from '../../util/DispatchUtil';
import {disablePrimarySeKnop} from '../../util/Util';
import {showWijzigingenOpgeslagenToast} from '../../util/ToastUtil';
import type {Amputatie} from '../../datatypes/Onderzoek';
import type {SeAction} from '../../actions/SeAction';

export const vorigeOnderzoekenConfirmBtnKind = (afspraak: Afspraak): VorigeOnderzoekenConfirmBtnKind => afspraak.status === 'INGESCHREVEN' ? 'Onderzoek starten' : 'Volgende';

const mapStateToProps = (state: State) => {
    return {
        magOnderzoeken: state.autorisatie.onderzoeken,
    };
};

const mapDispatchToProps = dispatch => {
    return {
        onConfirm(client: Client, afspraak: Afspraak): void {
            switch (vorigeOnderzoekenConfirmBtnKind(afspraak)) {
                case 'Onderzoek starten':
                    disablePrimarySeKnop();
                    let actions: Array<SeAction> = [];
                    const amputatie: ?Amputatie = client.vorigeOnderzoeken && client.vorigeOnderzoeken[0] && client.vorigeOnderzoeken[0].onderzoek
                        ? client.vorigeOnderzoeken[0].onderzoek.amputatie
                        : null;
                    const onderzoekStartenAction: SeAction = createActionOnderzoekStarten(afspraak.id, amputatie);
                    actions.push(onderzoekStartenAction);
                    dispatchActions(dispatch, ...actions);

                    dispatch(createActionNavigateToOnderzoek(client.id, afspraak.id, 'Visuele inspectie'));
                    putTransactionToScreenItCentraalPromise(afspraak, 'ONDERZOEK_STARTEN', ...actions).then(() => {
                        showWijzigingenOpgeslagenToast();
                    });
                    break;
                case 'Volgende':
                    disablePrimarySeKnop();
                    dispatch(createActionNavigateToOnderzoek(client.id, afspraak.id, 'Visuele inspectie'));
                    break;
                default:
                    break;
            }
        },
    };

};

const VorigeOnderzoekenContainer = connect(mapStateToProps, mapDispatchToProps)(VorigeOnderzoekenView);

export default VorigeOnderzoekenContainer;
