import type {SeGebruikersActions} from "../actions/SeGebruikersActions"
import {ADD_ALL_SE_GEBRUIKERS, ADD_SE_GEBRUIKER, CLEAR_SE_GEBRUIKERS} from "../actions/SeGebruikersActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const SeGebruikersReducer: Reducer<Map<string, string>, SeGebruikersActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	let result = new Map()
	if (!stateSlice) {
		stateSlice = result
	}
	switch (action.type) {
		case ADD_ALL_SE_GEBRUIKERS:
			result = new Map([...stateSlice, ...Object.entries(action.seGebruikers)])
			break
		case ADD_SE_GEBRUIKER:
			result = stateSlice
			result.set(String(action.instellingGebruikerId), action.displayName)
			break
		case CLEAR_SE_GEBRUIKERS:
		case CLEAR_CACHE:
			break
		default:
			result = stateSlice
			break
	}

	return result
}

export default SeGebruikersReducer