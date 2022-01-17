import {EnvironmentInfoActions, SET_ENVIRONMENTINFO} from "../actions/EnvironmentInfoActions"
import {EnvironmentInfo} from "../datatypes/EnvironmentInfo"
import {Reducer} from "redux"

const EnvironmentInfoReducer: Reducer<EnvironmentInfo | null, EnvironmentInfoActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case SET_ENVIRONMENTINFO:
			return action.environment
		default:
			return stateSlice
	}
}

export default EnvironmentInfoReducer