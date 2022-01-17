/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import * as React from "react"
import ReactDOM from "react-dom"
import {ReactKeycloakProvider} from "@react-keycloak/web"
import keycloak from "./utils/Keycloak"
import * as serviceWorker from "./serviceWorker"
import "./index.css"
import {Action, applyMiddleware, compose, createStore, Store} from "redux"
import cpReducers from "./reducers"
import {loadState, saveState} from "./utils/StorageUtil"
import {Provider, useDispatch} from "react-redux"
import {Router} from "react-router-dom"
import App from "./App"
import thunk, {ThunkDispatch} from "redux-thunk"
import {State} from "./datatypes/State"
import {cpHistory} from "./routes/routes"
import CpIdleTimer from "./components/idle_timer/CpIdleTimer"
import {AuthClientEvent} from "@react-keycloak/core/lib/types"
import {logout} from "./utils/LogoutUtil"
import { datadogRum } from '@datadog/browser-rum';

export type ReduxThunkDispatch = ThunkDispatch<State, any, Action>;

export function useThunkDispatch(): ReduxThunkDispatch {
    return useDispatch<ReduxThunkDispatch>()
}

const composeEnhancers = (process.env.NODE_ENV !== 'production' && (window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ )|| compose

export const cpStore: Store = createStore(cpReducers, loadState(), composeEnhancers(applyMiddleware(thunk)))
cpStore.subscribe(() => {
    saveState()
})

const automaticLogout = (authEvent: AuthClientEvent) => {
    if (authEvent === "onAuthRefreshError") {
        logout(keycloak, cpStore.dispatch, true)
    }
}

if (process.env.NODE_ENV !== 'production') {
	datadogRum.init({
		applicationId: '9247fc9b-d035-4f9f-9d74-f9140c41c834',
		clientToken: 'pub4c285cbd766641dd2f560ee6b9a9cb63',
		site: 'datadoghq.eu',
		service:'clientportaal',
		sampleRate: 100,
		trackInteractions: true,
		defaultPrivacyLevel: 'mask'
	});

	datadogRum.startSessionReplayRecording();
}

ReactDOM.render(
    <React.StrictMode>
        <ReactKeycloakProvider
            authClient={keycloak}
            initOptions={{checkLoginIframe: false, pkceMethod: "S256"}}
            onEvent={automaticLogout}>
            <Provider store={cpStore}>
                <Router history={cpHistory}>
                    <CpIdleTimer>
                        <App/>
                    </CpIdleTimer>
                </Router>
            </Provider>
        </ReactKeycloakProvider>
    </React.StrictMode>,
    document.getElementById("root"),
)

serviceWorker.unregister()
