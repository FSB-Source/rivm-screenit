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
import AkkoordPopupView from './AkkoordPopupView';
import {dispatchActions} from '../../../util/DispatchUtil';
import type {Popup} from '../../../datatypes/Popup';
import {createActionAkkoordPopup, createActionClearPopup} from '../../../actions/PopupActions';
import {connect} from 'react-redux';

const mapStateToProps = (state: State) => {
    const popup: Popup = state.popup;
    if (popup.visible === true) {
        return {
            titel: popup.titel,
            body: popup.body,
            callback: popup.callback,
            cancelCallback: popup.cancelCallback,
            visible: true,
            akkoordString: popup.akkoordString,
            annulerenString: popup.annulerenString,
        };
    } else {
        return {
            visible: false,
        };
    }
};

const mapDispatchToProps = dispatch => {
    return {
        akkoord(callback: Function) {
            callback();
            dispatchActions(dispatch, createActionAkkoordPopup());
        },
        cancel(cancelCallback: ?Function) {
            if (cancelCallback) {
                cancelCallback();
            }
            dispatchActions(dispatch, createActionClearPopup());
        },
    };
};

const AkkoordPopupContainer = connect(mapStateToProps, mapDispatchToProps)(AkkoordPopupView);

export default AkkoordPopupContainer;
