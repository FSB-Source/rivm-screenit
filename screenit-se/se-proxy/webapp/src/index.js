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
import {render} from 'react-dom';
import {Provider} from 'react-redux';
import {unregister} from './registerServiceWorker';
import './index.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';
import './screenit_se.scss';
import BuitensteView from './components/app/BuitensteView';
import {store} from './Store';
import {readEnvironmentInfo} from './restclient/EnvironmentInfoRestclient';
import {initWebSocket} from './util/WebSocketUtil';

const root = document.getElementById('root');

if (root) {
    render(
        <Provider store={store}>
            <BuitensteView/>
        </Provider>
        , root);

    initWebSocket();

    readEnvironmentInfo();

    unregister(); 
}
