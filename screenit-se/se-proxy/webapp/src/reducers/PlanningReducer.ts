import type {PlanningActions} from "../actions/PlanningActions"
import {CLEAR_PLANNING, VUL_PLANNING} from "../actions/PlanningActions"
import type {Planning} from "../datatypes/Planning"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

type PlanningMap = Map<string, Planning>;

const PlanningReducer: Reducer<PlanningMap, PlanningActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<string, Planning> = new Map()
	if (action.type === VUL_PLANNING) {
		const planning: Planning = action.planning
		result.set(action.datum, planning)
		return new Map([...stateSlice, ...result])
	} else if (action.type === CLEAR_PLANNING) {
		stateSlice.delete(action.datum)
		return stateSlice
	} else if (action.type === CLEAR_CACHE) {
		return new Map()
	}
	return stateSlice
}

export default PlanningReducer