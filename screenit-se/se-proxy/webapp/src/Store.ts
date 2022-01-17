import {compose, createStore} from "redux"
import seReducers from "./reducers"

declare global {
	interface Window {
		__REDUX_DEVTOOLS_EXTENSION_COMPOSE__?: typeof compose;
	}
}

export type RootState = ReturnType<typeof store.getState>
export type AppDispatch = typeof store.dispatch

const composeEnhancers = (window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose
export const store = createStore(seReducers, composeEnhancers())
