import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import type {OpgehaaldeDagenActions} from "../actions/OpgehaaldeDagenActions"
import {DAGLIJST_OPGEHAALD} from "../actions/OpgehaaldeDagenActions"
import {Reducer} from "redux"

const OpgehaaldeDagenReducer: Reducer<Set<string>, OpgehaaldeDagenActions | ClearCacheActions> = (stateSlice = new Set(), action) => {
	switch (action.type) {
		case DAGLIJST_OPGEHAALD:
			return new Set([...stateSlice, action.datum])
		case CLEAR_CACHE:
			return new Set()
		default:
			return stateSlice
	}
}

export default OpgehaaldeDagenReducer