import type {ImsActions} from "../actions/ImsActions"
import {SET_STUDY_FOR_IMS} from "../actions/ImsActions"
import {Reducer} from "redux"

const ImsReducer: Reducer<number | null, ImsActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case SET_STUDY_FOR_IMS:
			return action.activeStudyForIms !== undefined ? action.activeStudyForIms : null
		default:
			return stateSlice
	}
}

export default ImsReducer