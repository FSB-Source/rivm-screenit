import type {Huisarts} from "../datatypes/Huisarts"
import type {HuisartsActions} from "../actions/HuisartsActions"
import {VUL_HUISARTSEN_BY_ID} from "../actions/HuisartsActions"
import type {ClearCacheActions} from "../actions/ClearCacheActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const HuisartsenReducer: Reducer<Map<number, Huisarts>, HuisartsActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, Huisarts> = new Map()
	switch (action.type) {
		case VUL_HUISARTSEN_BY_ID:
			action.huisartsen.map((h: Huisarts) => result.set(h.id, h))
			break
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}
	return new Map([...stateSlice, ...result])
}

export default HuisartsenReducer