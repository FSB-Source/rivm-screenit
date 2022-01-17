import type {BezigMetKwaliteitsopnameAction} from "../actions/BezigMetKwaliteitsopnameActions"
import {BEEINDIG_BEZIG_MET_KWALITEITSOPNAME, START_BEZIG_MET_KWALITEITSOPNAME} from "../actions/BezigMetKwaliteitsopnameActions"
import {Reducer} from "redux"

const BezigMetKwaliteitsopnameVolgnrReducer: Reducer<number | null, BezigMetKwaliteitsopnameAction> = (stateSlice = null, action) => {
	switch (action.type) {
		case START_BEZIG_MET_KWALITEITSOPNAME:
			return action.volgnr !== undefined ? action.volgnr : null
		case BEEINDIG_BEZIG_MET_KWALITEITSOPNAME:
			return null
		default:
			return stateSlice
	}
}

export default BezigMetKwaliteitsopnameVolgnrReducer