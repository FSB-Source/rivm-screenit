import type {SessionActions} from "../actions/SessionActions"
import {CLEAR_SESSION, SET_SESSION} from "../actions/SessionActions"
import type {Session} from "../datatypes/Session"
import {Reducer} from "redux"
import {ClearCacheActions} from "../actions/ClearCacheActions"

const SessionReducer: Reducer<Session | null, SessionActions | ClearCacheActions> = (stateSlice = null, action) => {
	switch (action.type) {
		case SET_SESSION:
			return {
				gebruikersnaam: action.gebruikersnaam,
				medewerkercode: action.medewerkercode,
				displayName: action.displayName,
				seCode: action.seCode,
				seNaam: action.seNaam,
				yubikeyIdentificatie: action.yubikeyIdentificatie,
				instellingGebruikerId: action.instellingGebruikerId,
			}
		case CLEAR_SESSION:
			return null
		default:
			return stateSlice
	}
}

export default SessionReducer