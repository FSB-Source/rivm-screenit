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
import {connect} from 'react-redux';
import type {KwaliteitsopnameReden, VoorOfNaKalibratie} from './KwaliteitsopnameView';
import KwaliteitsopnameView from './KwaliteitsopnameView';
import {putTransactionToScreenItCentraalPromise} from '../../restclient/TransactionRestclient';
import {BEEINDIG_KWALITEITSOPNAME, createActionKwaliteitsopnameOrm, START_KWALITEITSOPNAME} from '../../actions/KwaliteitsopnameOrmActions';
import {store} from '../../Store';
import {beeindigKwaliteitsopname, kwaliteitsopnameToevoegenAanWerklijst} from '../../restclient/WerklijstRestclient';
import {aeTitle, seCode} from '../../util/Util';
import {fetchApiPromise} from '../../util/ApiUtil';
import {createBeeindigBezigMetKwaliteitsopnameAction, createStartBezigMetKwaliteitsopnameAction} from '../../actions/BezigMetKwaliteitsopnameActions';
import {createActionNavigateToDaglijst} from '../../actions/NavigationActions';
import {nu, nuISO} from '../../util/DateUtil';

const mapStateToProps = () => ({});

const mapDispatchToProps = dispatch => {
    return {
        onKwaliteitsopnameAction(actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie): void {
            const mammograafnr: string = aeTitle().substr(aeTitle().length - 1, 1);
            if (actionType === START_KWALITEITSOPNAME) {

                fetchApiPromise('GET', 'kwaliteitsopname/' + mammograafnr).then(response => {
                    response.json().then(volgnr => {
                        startOfBeeindigKwaliteitsopname(mammograafnr, volgnr, actionType, reden, voorOfNaKalibratie, dispatch);
                    });
                });
            } else { 
                startOfBeeindigKwaliteitsopname(mammograafnr, store.getState().bezigMetKwaliteitsopnameVolgnr, actionType, reden, voorOfNaKalibratie, dispatch);
            }
        },
    };
};

const startOfBeeindigKwaliteitsopname = (
    mammograafnr: string, volgnr: number, actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie, dispatch: any) => {
    const medewerkercode: string = store.getState().session.medewerkercode;
    const datumAcccesionNumber: string = nu().format('DDMMYY'); 
    const datumPatientId: string = nu().format('YYYYMMDD');
    const startMoment: string = nuISO();
    const qcOfBu: string = (reden === 'Vervanging rontgenbuis' ? 'BU' : 'QC');
    const kalibratieLetter: string = (voorOfNaKalibratie === 'Na kalibratie' ? 'N' : (voorOfNaKalibratie === 'Voor kalibratie' ? 'V' : 'G'));
    const seNr: string = aeTitle().substr(7, 3);
    const volgnrString: string = (volgnr < 10 ? '0' : '') + volgnr;
    const accessionNumber: string = qcOfBu + seNr + 'M' + mammograafnr + datumAcccesionNumber + kalibratieLetter + volgnrString;
    const onderzoekscode: string = 'LRCB' + qcOfBu;
    const patientId: string = seCode() + '_LRCB_' + datumPatientId;

    switch (actionType) {
        case START_KWALITEITSOPNAME:
            dispatch(createStartBezigMetKwaliteitsopnameAction(volgnr));
            kwaliteitsopnameToevoegenAanWerklijst({
                aeTitle: aeTitle(),
                medewerkercode,
                reden,
                voorOfNaKalibratie,
                seCode: seCode(),
                accessionNumber,
                onderzoekscode,
                startMoment,
                patientId,
            });
            break;
        case BEEINDIG_KWALITEITSOPNAME:
            dispatch(createBeeindigBezigMetKwaliteitsopnameAction(volgnr));
            beeindigKwaliteitsopname(aeTitle());
            break;
    }
    const actionOrm = createActionKwaliteitsopnameOrm(actionType, seCode(), reden, patientId, accessionNumber, onderzoekscode);
    dispatch(actionOrm);
    putTransactionToScreenItCentraalPromise(null, actionType === START_KWALITEITSOPNAME ? 'START_KWALITEITSOPNAME_TRANSACTION' : 'BEEINDIG_KWALITEITSOPNAME_TRANSACTION',
        actionOrm);

    if (actionType === BEEINDIG_KWALITEITSOPNAME) {
        dispatch(createActionNavigateToDaglijst());
    }
};

const KwaliteitsopnameContainer = connect(mapStateToProps, mapDispatchToProps)(KwaliteitsopnameView);

export default KwaliteitsopnameContainer;
