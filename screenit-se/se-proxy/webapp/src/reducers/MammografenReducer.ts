import {Mammograaf} from "../datatypes/Mammograaf"
import type {VulMammografenAction} from "../actions/MammograafActions"
import {VUL_MAMMOGRAFEN} from "../actions/MammograafActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const MammografenReducer: Reducer<Map<number, Mammograaf>, VulMammografenAction | ClearCacheActions> = (stateSlice = new Map(), action) => {
	switch (action.type) {
		case VUL_MAMMOGRAFEN:
			return action.mammografen.reduce((result: Map<number, Mammograaf>, mammograaf: Mammograaf) => {
				return result.set(mammograaf.id, mammograaf)
			}, new Map())
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}
}

export default MammografenReducer