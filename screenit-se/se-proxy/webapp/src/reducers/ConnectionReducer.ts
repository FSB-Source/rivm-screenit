import type {ConnectionActions} from "../actions/ConnectionActions"
import {SET_OFFLINE, SET_ONLINE} from "../actions/ConnectionActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const ConnectionReducer: Reducer<boolean, ConnectionActions | ClearCacheActions> = (stateSlice = false, action) => {
	switch (action.type) {
		case SET_ONLINE:
			return true
		case SET_OFFLINE:
			return false
		case CLEAR_CACHE:
			return false
		default:
			return stateSlice
	}
}

export default ConnectionReducer