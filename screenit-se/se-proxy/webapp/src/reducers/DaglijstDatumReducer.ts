import type {DaglijstDatumActions} from "../actions/DaglijstDatumActions"
import {KIES_DAGLIJST_DATUM} from "../actions/DaglijstDatumActions"
import {vandaagISO} from "../util/DateUtil"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const DaglijstDatumReducer: Reducer<string, DaglijstDatumActions | ClearCacheActions> = (stateSlice = vandaagISO(), action) => {
	switch (action.type) {
		case KIES_DAGLIJST_DATUM:
			return action.datum
		case CLEAR_CACHE:
			return vandaagISO()
		default:
			return stateSlice
	}
}

export default DaglijstDatumReducer