import type {Dagverslag} from "../datatypes/Dagverslag"
import type {DagverslagActions} from "../actions/DagverslagActions"
import {SET_DAGVERSLAG} from "../actions/DagverslagActions"
import type {ClearCacheActions} from "../actions/ClearCacheActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const defaultDagverslag: Map<string, Dagverslag> = new Map()

const DagverslagReducer: Reducer<Map<string, Dagverslag>, DagverslagActions | ClearCacheActions> = (stateSlice = defaultDagverslag, action) => {
	switch (action.type) {
		case SET_DAGVERSLAG:
			const dagverslag = action.dagverslag
			stateSlice = stateSlice.set(action.datum, dagverslag)
			return new Map(stateSlice)
		case CLEAR_CACHE:
			return defaultDagverslag
		default:
			return stateSlice
	}
}

export default DagverslagReducer