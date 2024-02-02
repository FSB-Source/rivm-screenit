/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import "./styling/scss/style.scss"
import React from "react"
import {Provider, TypedUseSelectorHook, useDispatch, useSelector} from "react-redux"
import {configureStore, ThunkDispatch} from "@reduxjs/toolkit"
import {AnyAction} from "redux"
import {AppRoutes} from "./routes"
import App from "./App"
import {loadState, saveState} from "./util/StorageUtil"
import RootReducer from "./state"
import {BrowserRouter} from "react-router-dom"
import createCache from "@emotion/cache"
import {CacheProvider} from "@emotion/react"
import {createRoot} from "react-dom/client"
import IdleTimerWrapper from "./components/IdleTimerWrapper"
import {NonceProvider} from "react-select"

export const store = configureStore({
	reducer: RootReducer,
	preloadedState: loadState(),
})

store.subscribe(() => {
	saveState()
})

export type AppState = ReturnType<typeof store.getState>
type AppDispatch = typeof store.dispatch
export type AppThunkDispatch = ThunkDispatch<AppState, any, AnyAction>;

export const useAppSelector: TypedUseSelectorHook<AppState> = useSelector
export const useAppDispatch = () => useDispatch<AppDispatch>()
export const useAppThunkDispatch = () => useDispatch<AppThunkDispatch>()

interface NonceElement extends Element {
	content: string
}

const nonce = (document.querySelector("meta[property=\"csp-nonce\"]") as NonceElement).content || ""

const root = createRoot(document.getElementById("root")!)
root.render(
	<Provider store={store}>
		<NonceProvider nonce={nonce} cacheKey={"emotion-cache"}>
			<BrowserRouter>
				<IdleTimerWrapper>
					<App>
						<AppRoutes/>
					</App>
				</IdleTimerWrapper>
			</BrowserRouter>
		</NonceProvider>
	</Provider>
)
