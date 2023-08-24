import renderer, {ReactTestRenderer} from "react-test-renderer"
import configureStore, {MockStoreEnhanced} from "redux-mock-store"
import {Middleware} from "redux"

export function rerender(component: ReactTestRenderer): renderer.ReactTestRendererJSON {
	return component.toJSON() as renderer.ReactTestRendererJSON
}

export function geefMockStoreMetRootState(initialState: {}): MockStoreEnhanced<unknown> {
	const middlewares: Middleware[] = []
	const mockStore = configureStore(middlewares)

	return mockStore(initialState)
}
