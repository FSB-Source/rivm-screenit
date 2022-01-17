import type {NavigationActions} from "../actions/NavigationActions"
import {
	CLEAR_NAVIGATION,
	NAVIGATE_TO_CLIENTGEGEVENS,
	NAVIGATE_TO_CONNECTIESTATUS,
	NAVIGATE_TO_DAGLIJST,
	NAVIGATE_TO_DAGVERSLAG,
	NAVIGATE_TO_KWALITEITSOPNAME,
	NAVIGATE_TO_ONDERZOEK,
} from "../actions/NavigationActions"
import type {NavigationState} from "../datatypes/Navigation"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const daglijstNavigationState: NavigationState = {
	tab: "Daglijst",
	subPagina: undefined,
	clientId: undefined,
	afspraakId: undefined,
}
const geenNavigationState: NavigationState = {
	tab: "Geen",
	subPagina: undefined,
	clientId: undefined,
	afspraakId: undefined,
}

const NavigationReducer: Reducer<NavigationState, NavigationActions | ClearCacheActions> = (stateSlice = geenNavigationState, action) => {
	switch (action.type) {
		case NAVIGATE_TO_DAGLIJST:
			return daglijstNavigationState
		case NAVIGATE_TO_CLIENTGEGEVENS:
			return {
				tab: "Cliëntgegevens",
				subPagina: undefined,
				clientId: action.clientId,
				afspraakId: action.afspraakId,
			}
		case NAVIGATE_TO_ONDERZOEK:
			return {
				tab: "Onderzoek",
				subPagina: action.subPagina,
				clientId: action.clientId,
				afspraakId: action.afspraakId,
			}
		case NAVIGATE_TO_DAGVERSLAG:
			return {
				tab: "Dagverslag",
				subPagina: undefined,
				clientId: undefined,
				afspraakId: undefined,
			}
		case NAVIGATE_TO_KWALITEITSOPNAME:
			return {
				tab: "Kwaliteitsopname",
				subPagina: undefined,
				clientId: undefined,
				afspraakId: undefined,
			}
		case NAVIGATE_TO_CONNECTIESTATUS:
			return {
				tab: "Connectiestatus",
				subPagina: undefined,
				clientId: undefined,
				afspraakId: undefined,
			}
		case CLEAR_NAVIGATION:
			return geenNavigationState
		case CLEAR_CACHE:
			return geenNavigationState
		default:
			return stateSlice
	}
}

export default NavigationReducer
