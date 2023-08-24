/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {ReactKeycloakProvider} from "@react-keycloak/web"
import keycloak from "./utils/Keycloak"
import * as serviceWorker from "./serviceWorker"
import "./index.css"
import {Action, applyMiddleware, compose, createStore, Store} from "redux"
import cpReducers from "./reducers"
import {loadState, saveState} from "./utils/StorageUtil"
import {Provider, useDispatch} from "react-redux"
import {BrowserRouter} from "react-router-dom"
import App from "./App"
import thunk, {ThunkDispatch} from "redux-thunk"
import {State} from "./datatypes/State"
import IdleTimerWrapper from "./wrapper/IdleTimerWrapper"
import {AuthClientEvent} from "@react-keycloak/core/lib/types"
import {datadogRum} from "@datadog/browser-rum"
import AuthenticationWrapper from "./wrapper/AuthenticationWrapper"
import {setLoggingOutAction} from "./actions/AuthenticatieAction"
import {createRoot} from "react-dom/client"
import createCache from "@emotion/cache"
import {CacheProvider} from "@emotion/react"
import ErrorBoundary from "./components/error_boundary/ErrorBoundary"

export type ReduxThunkDispatch = ThunkDispatch<State, any, Action>;

export function useThunkDispatch(): ReduxThunkDispatch {
	return useDispatch<ReduxThunkDispatch>()
}

const composeEnhancers = (process.env.NODE_ENV !== "production" && (window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__) || compose

export const cpStore: Store = createStore(cpReducers, loadState(), composeEnhancers(applyMiddleware(thunk)))
cpStore.subscribe(() => {
	saveState()
})

const automaticLogout = (authEvent: AuthClientEvent) => {
	if (authEvent === "onAuthRefreshError") {
		cpStore.dispatch(setLoggingOutAction(true))
	}
}

if (process.env.NODE_ENV !== "production") {
	datadogRum.init({
		applicationId: "9247fc9b-d035-4f9f-9d74-f9140c41c834",
		clientToken: "pub4c285cbd766641dd2f560ee6b9a9cb63",
		site: "datadoghq.eu",
		service: "clientportaal",
		sampleRate: 100,
		trackInteractions: true,
		defaultPrivacyLevel: "mask",
	})

	datadogRum.startSessionReplayRecording()
}

const emotionCache = createCache({
	key: "emotion-cache",
	nonce: document.querySelector("meta[property=\"csp-nonce\"]")?.content || "",
})
const component = document.getElementById("root")
const root = createRoot(component!)

root.render(
	<ReactKeycloakProvider
		authClient={keycloak}
		initOptions={{checkLoginIframe: false, pkceMethod: "S256"}}
		onEvent={automaticLogout}>
		<React.StrictMode>
			<ErrorBoundary>
				<Provider store={cpStore}>
					<CacheProvider value={emotionCache}>
						<BrowserRouter>
							<AuthenticationWrapper>
								<IdleTimerWrapper>
									<App/>
								</IdleTimerWrapper>
							</AuthenticationWrapper>
						</BrowserRouter>
					</CacheProvider>
				</Provider>
			</ErrorBoundary>
		</React.StrictMode>
	</ReactKeycloakProvider>)

serviceWorker.unregister()
