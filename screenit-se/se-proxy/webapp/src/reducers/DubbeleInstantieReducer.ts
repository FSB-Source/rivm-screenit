import type {DubbeleInstantieActions} from "../actions/DubbeleInstantieActions"
import {SET_DUBBELE_INSTANTIE} from "../actions/DubbeleInstantieActions"
import {Reducer} from "redux"

const DubbeleInstantieReducer: Reducer<boolean, DubbeleInstantieActions> = (stateSlice = false, action) => {
	switch (action.type) {
		case SET_DUBBELE_INSTANTIE:
			return true
		default:
			return stateSlice
	}
}

export default DubbeleInstantieReducer