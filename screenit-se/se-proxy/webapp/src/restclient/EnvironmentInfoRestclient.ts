import {store} from "../Store"
import {createActionSetEnvironmentInfo} from "../actions/EnvironmentInfoActions"
import {fetchApi} from "../util/ApiUtil"
import type {EnvironmentInfo} from "../datatypes/EnvironmentInfo"

export const readEnvironmentInfo = (): void => {
	fetchApi("GET", "environmentInfo", (environment: EnvironmentInfo) => {
		store.dispatch(createActionSetEnvironmentInfo(environment))
	})
}