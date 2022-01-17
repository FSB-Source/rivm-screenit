import type {Recht} from "../datatypes/Recht"
import {AutorisatieActions, SET_AUTORISATIE} from "../actions/AutorisatieActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {isAuthorized} from "../util/AutorisatieUtil"
import {Reducer} from "redux"

const defaultRecht: Recht = {
	inschrijven: true,
	onderzoeken: false,
	signaleren: false,
	kwaliteitsopname: false,
	connectiestatus: false,
}

const AutorisatieReducer: Reducer<Recht, AutorisatieActions | ClearCacheActions> = (stateSlice = defaultRecht, action) => {
	switch (action.type) {
		case SET_AUTORISATIE:
			return {
				inschrijven: isAuthorized(action.rechten.inschrijvenRecht),
				onderzoeken: isAuthorized(action.rechten.onderzoekenRecht),
				signaleren: isAuthorized(action.rechten.signalerenRecht),
				kwaliteitsopname: isAuthorized(action.rechten.kwaliteitsopnameRecht),
				connectiestatus: isAuthorized(action.rechten.connectiestatusRecht),
			}
		case CLEAR_CACHE:
			return defaultRecht
		default:
			return stateSlice
	}
}

export default AutorisatieReducer