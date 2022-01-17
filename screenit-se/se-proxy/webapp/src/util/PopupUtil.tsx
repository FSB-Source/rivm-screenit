import {createActionShowPopup} from "../actions/PopupActions"
import WijzigingenOngedaanPopupView from "../components/melding/WijzigingenOngedaanPopupView"
import React from "react"
import AmputatieWaarschuwingPopup from "../components/melding/AmputatieWaarschuwingPopupView"

export const showWijzigingenPopup = (callback: (...args: Array<any>) => any, dispatch: any): void => {
	dispatch(createActionShowPopup("Pagina verlaten?", <WijzigingenOngedaanPopupView/>, callback, undefined, "Verlaten", "Annuleren"))
}

export const showAmputatieWaarschuwingPopup = (callback: (...args: Array<any>) => any, dispatch: any): void => {
	dispatch(createActionShowPopup("Amputatiekruis controle", <AmputatieWaarschuwingPopup/>, callback, undefined, "Gecontroleerd, toch doorgaan", "Annuleren"))
}