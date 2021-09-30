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
import {BrowserRouter} from 'react-router-dom';
import TabbarContainer from './TabbarContainer';
import type {Session} from '../../datatypes/Session';
import {ToastContainer} from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';
import {DEFAULT_TOAST_TIMEOUT} from '../../util/ToastUtil';
import WerkstationkiezerContainer from '../login/WerkstationkiezerContainer';
import {Mammograaf} from '../../datatypes/Mammograaf';
import {store} from '../../Store';
import LoginContainer from '../login/LoginContainer';

export type AppProps = {
    session: Session;
    mammograafId: ?number;
    mammografenById: Map<number, Mammograaf>;
}

const AppView = (props: AppProps) =>
    (<div onKeyDown={onKeyPressed}>
        {props.session === null ? (
                <div>
                    <LoginContainer/>
                </div>
            )
            : (props.mammograafId === null && props.mammografenById.size &&
                (store.getState().environmentInfo.environment === 'Test' || store.getState().environmentInfo.environment === 'PAT') ?
                    (<WerkstationkiezerContainer/>)
                    :
                    (
                        <BrowserRouter>
                            <div className="app">
                                <TabbarContainer/>
                            </div>
                        </BrowserRouter>
                    )
            )}
        <ToastContainer autoClose={DEFAULT_TOAST_TIMEOUT} hideProgressBar position={'top-center'}/>
    </div>);

const onKeyPressed = (e) => {

    return !(e.ctrlKey && e.keyCode === 74);
};

export default AppView;
