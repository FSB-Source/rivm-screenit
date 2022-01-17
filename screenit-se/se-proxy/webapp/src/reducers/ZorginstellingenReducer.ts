import type {Zorginstelling} from "../datatypes/Zorginstelling"
import type {ZorginstellingActions} from "../actions/ZorginstellingActions"
import {VUL_ZORGINSTELLINGEN} from "../actions/ZorginstellingActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const ZorginstellingenReducer: Reducer<Map<number, Zorginstelling>, ZorginstellingActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, Zorginstelling> = new Map()

	switch (action.type) {
		case VUL_ZORGINSTELLINGEN:
			action.zorginstellingen.map((z: Zorginstelling) => result.set(z.id, z))
			break
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}

	return new Map([...stateSlice, ...result])
}

export default ZorginstellingenReducer