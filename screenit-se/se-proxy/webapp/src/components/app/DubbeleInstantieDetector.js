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

import React, {Component} from 'react';
import PropTypes from 'prop-types';
import {dispatchActions} from '../../util/DispatchUtil';
import {store} from '../../Store';
import {createActionDubbeleInstantie} from '../../actions/DubbeleInstantieActions';
import {MELDING_DUBBELE_INSTANTIE, showErrorToastWithoutAutoClose} from '../../util/ToastUtil';

const SE_VENSTER_GEOPEND = 'SE_VENSTER_GEOPEND';
const SE_SLUIT_DUBBEL_VENSTER = 'SE_SLUIT_DUBBEL_VENSTER';

export default class DubbeleInstantieDetector extends Component<PropTypes, any> {

    constructor(props: PropTypes) {
        super(props);
        this.handlelocalStorageUpdated = this.handlelocalStorageUpdated.bind(this);
    }

    componentDidMount() {
        localStorage.setItem(SE_VENSTER_GEOPEND, Date.now().toString()); 
        window.addEventListener('storage', this.handlelocalStorageUpdated);
    }

    handlelocalStorageUpdated = (event: any) => {
        console.log('Local storage event: ' + event.key);
        if (event.key === SE_VENSTER_GEOPEND) {

            localStorage.setItem(SE_SLUIT_DUBBEL_VENSTER, Date.now().toString());
        }
        if (event.key === SE_SLUIT_DUBBEL_VENSTER) {
            this.removeEventListener(); 
            dispatchActions(store.dispatch, createActionDubbeleInstantie());
            showErrorToastWithoutAutoClose(MELDING_DUBBELE_INSTANTIE);
        }
    };

    removeEventListener() {
        window.removeEventListener('storage', this.handlelocalStorageUpdated);
    }

    componentWillUnmount() {
        this.removeEventListener();
    }

    render() {
        return null;
    }
}
