import {store} from "../Store"
import {createActionSetEnvironmentInfo} from "../actions/EnvironmentInfoActions"
import {fetchApi} from "../util/ApiUtil"
import type {EnvironmentInfo} from "../datatypes/EnvironmentInfo"
import {startPollingNfc} from "../util/NfcUtil"

export const readEnvironmentInfo = (): void => {
	fetchApi("GET", "environmentInfo", (environment: EnvironmentInfo) => {
		store.dispatch(createActionSetEnvironmentInfo(environment))
		if (environment.nfcEnabled) {
			startPollingNfc()
		}
	})
}