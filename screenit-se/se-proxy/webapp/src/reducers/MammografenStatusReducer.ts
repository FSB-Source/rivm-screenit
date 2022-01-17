import type {MammografenStatus} from "../datatypes/connectiestatus/MammografenStatus"
import type {MammografenStatusActions} from "../actions/MammografenStatusActions"
import {VUL_MAMMOGRAFEN_STATUS} from "../actions/MammografenStatusActions"
import {Reducer} from "redux"

const MammografenStatusReducer: Reducer<MammografenStatus, MammografenStatusActions> = (stateSlice: MammografenStatus = [], action: MammografenStatusActions) => {
	switch (action.type) {
		case VUL_MAMMOGRAFEN_STATUS:
			return action.statusList
		default:
			return stateSlice
	}
}

export default MammografenStatusReducer