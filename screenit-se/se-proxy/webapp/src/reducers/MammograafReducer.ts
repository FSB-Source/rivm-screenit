import type {MammograafActions} from "../actions/MammograafActions"
import {SET_HUIDIGE_MAMMOGRAAF} from "../actions/MammograafActions"
import type {SessionActions} from "../actions/SessionActions"
import {CLEAR_SESSION} from "../actions/SessionActions"
import type {ClearCacheActions} from "../actions/ClearCacheActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const MammograafReducer: Reducer<number | null, MammograafActions | SessionActions | ClearCacheActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case SET_HUIDIGE_MAMMOGRAAF:
			return action.mammograafId
		case CLEAR_SESSION:
		case CLEAR_CACHE:
			return null
		default:
			return stateSlice
	}
}

export default MammograafReducer