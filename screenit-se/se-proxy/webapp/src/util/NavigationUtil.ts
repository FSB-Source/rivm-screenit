import {Dispatch} from "redux"
import {
	createActionNavigateToClientgegevens,
	createActionNavigateToConnectiestatus,
	createActionNavigateToDaglijst,
	createActionNavigateToDagverslag,
	createActionNavigateToKwaliteitsopname,
	createActionNavigateToOnderzoek,
	NavigationActions,
} from "../actions/NavigationActions"
import {putNavigationState} from "../restclient/NavigationStateRestClient"
import {SubPagina} from "../datatypes/Navigation"

export const navigateToDaglijst = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToDaglijst())
}

export const navigateToClientgegevens = (dispatch: Dispatch, clientId: number, afspraakId: number): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToClientgegevens(clientId, afspraakId))
}

export const navigateToOnderzoek = (dispatch: Dispatch, clientId: number, afspraakId: number, subPagina: SubPagina): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToOnderzoek(clientId, afspraakId, subPagina))
}

export const navigateToDagverslag = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToDagverslag())
}

export const navigateToKwaliteitsopname = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToKwaliteitsopname())
}

export const navigateToConnectiestatus = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToConnectiestatus())
}

export const restoreNavigation = (dispatch: Dispatch, navigatieActie: NavigationActions): void => {
	toProxyAndDispatch(dispatch, navigatieActie)
}

function toProxyAndDispatch(dispatchfunctie: Dispatch, action: NavigationActions): void {
	putNavigationState(action)
	dispatchfunctie(action)
}
