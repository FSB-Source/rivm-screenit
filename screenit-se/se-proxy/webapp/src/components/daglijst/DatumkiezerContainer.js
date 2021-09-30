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
import {createActionKiesDaglijstDatum} from '../../actions/DaglijstDatumActions';
import DatumkiezerView from './DatumkiezerView';
import {leesAfspraken} from '../../restclient/DaglijstRestclient';
import type {State} from '../../datatypes/State';
import {leesPlanning} from '../../restclient/PlanningRestClient';
import {getDagverslag} from '../../restclient/DagverslagRestClient';
import {createActionNavigateToDaglijst, createActionNavigateToDagverslag} from '../../actions/NavigationActions';
import {datumInVerleden} from '../../util/DateUtil';
import {showErrorToast} from '../../util/ToastUtil';

const mapStateToProps = (state: State) => {
    return {
        daglijstDatum: state.daglijstDatum,
        online: state.online,
    };
};

const mapDispatchToProps = dispatch => {
    return {
        onChooseDate(newDate: string, online: boolean): void {
            if (datumInVerleden(newDate)) {
                if (online) {
                    dispatch(createActionKiesDaglijstDatum(newDate));
                    leesAfspraken(newDate, createActionNavigateToDagverslag());
                    leesPlanning(newDate);
                    getDagverslag(newDate);
                } else {
                    showErrorToast('Dagverslag kan niet worden geopend, SE is offline.');
                }
            } else {
                dispatch(createActionKiesDaglijstDatum(newDate));
                leesAfspraken(newDate, createActionNavigateToDaglijst());
                leesPlanning(newDate);
            }
        },
    };
};

const DatumkiezerContainer = connect(mapStateToProps, mapDispatchToProps)(DatumkiezerView);

export default DatumkiezerContainer;
