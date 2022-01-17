import type {DagverslagActions} from "../actions/DagverslagActions"
import {SET_NIET_AFGESLOTEN_DATUM} from "../actions/DagverslagActions"
import type {ClearCacheActions} from "../actions/ClearCacheActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const NietAfgeslotenVanafReducer: Reducer<string | null, DagverslagActions | ClearCacheActions> = (stateSlice = null, action: DagverslagActions | ClearCacheActions) => {
	switch (action.type) {
		case SET_NIET_AFGESLOTEN_DATUM:
			return action.nietAfgeslotenVanaf !== undefined ? action.nietAfgeslotenVanaf : null
		case CLEAR_CACHE:
			return null
		default:
			return stateSlice
	}
}

export default NietAfgeslotenVanafReducer