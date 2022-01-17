import type {ErrorActions} from "../actions/ErrorActions"
import {FATAL_ERROR} from "../actions/ErrorActions"
import type {ErrorDto} from "../datatypes/ErrorDto"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const ErrorReducer: Reducer<ErrorDto | null, ErrorActions | ClearCacheActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case FATAL_ERROR:
			return action.error
		case CLEAR_CACHE:
			return null
		default:
			return stateSlice
	}
}

export default ErrorReducer