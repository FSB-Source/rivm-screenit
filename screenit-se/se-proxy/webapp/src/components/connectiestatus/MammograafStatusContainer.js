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
import type {State} from '../../datatypes/State';
import MammograafStatusView from './MammograafStatusView';
import type {MammograafStatus} from '../../datatypes/connectiestatus/MammograafStatus';
import type {ConnectieStatusLevel} from '../../datatypes/connectiestatus/ConnectieStatus';
import {Dispatch} from 'redux';
import {createActionPutMammograafConnectieStatus} from '../../actions/ConnectieStatusActions';
import {getMandatory} from '../../util/MapUtil';
import {createActionShowPopup} from '../../actions/PopupActions';
import MammograafDicomFoutmeldingenPopupView from './MammograafDicomFoutmeldingenPopupView';
import React from 'react';
import type {MammograafDicomMessageType} from '../../datatypes/connectiestatus/MammograafDicomMessageError';

export type MammograafStatusContainerProps = {
    mammograafStatus: MammograafStatus;
}

const mapStateToProps = (state: State, ownProps: MammograafStatusContainerProps) => {
    return {
        mammograafStatus: ownProps.mammograafStatus,
        mammograafConnectieStatusLevel: getMandatory(state.connectieStatus.mammograafConnectieStatusByAeTitle, ownProps.mammograafStatus.aeTitle),
    };
};

const mapDispatchToProps = (dispatch: Dispatch) => {
    return {
        reportStatusLevel: (aeTitle: string, statusLevel: ConnectieStatusLevel): void => {
            dispatch(createActionPutMammograafConnectieStatus(aeTitle, statusLevel));
        },
        toonDicomFouten: (messageType: MammograafDicomMessageType, mammograafStatus: MammograafStatus): void => {
            dispatch(createActionShowPopup(messageType + ' fouten op ' + mammograafStatus.aeTitle,
                <MammograafDicomFoutmeldingenPopupView messageType={messageType} errors={messageType === 'DMWL' ?
                    mammograafStatus.foutenSindsLaatsteSuccesDmwlBericht : mammograafStatus.foutenSindsLaatsteSuccesMppsBericht}/>,
                () => {
                }, () => {
                }, null, null));
        },
    };
};

const MammograafStatusContainer = connect(mapStateToProps, mapDispatchToProps)(MammograafStatusView);

export default MammograafStatusContainer;
