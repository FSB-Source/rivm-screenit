import type {PendingUpdates} from "../datatypes/PendingUpdates"
import type {UpdateActions} from "../actions/UpdateAction"
import {QUEUE_DAGLIJST_VERVERSEN} from "../actions/UpdateAction"
import {CLEAR_AFSPRAKEN} from "../actions/AfspraakActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {vandaagISO} from "../util/DateUtil"
import {Reducer} from "redux"

const PendingUpdatesReducer: Reducer<PendingUpdates | null, UpdateActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case QUEUE_DAGLIJST_VERVERSEN:
			return {
				moetDaglijstNogVerversen: true,
			}
		case CLEAR_AFSPRAKEN:
			return action.datum !== vandaagISO() ? stateSlice : {
				moetDaglijstNogVerversen: false,
			}
		case CLEAR_CACHE:
			return null
		default:
			return stateSlice
	}
}

export default PendingUpdatesReducer