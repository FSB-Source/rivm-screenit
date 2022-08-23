import type {PopupActions} from "../actions/PopupActions"
import {AKKOORD_POPUP, CLEAR_POPUP, SHOW_POPUP} from "../actions/PopupActions"
import type {Popup} from "../datatypes/Popup"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const emptyPopup: Popup = {
	titel: "",
	body: undefined,
	visible: false,
	annulerenString: "Annuleren",
	akkoordString: "Akkoord",
	callback: undefined,
	cancelCallback: undefined,
	alleenOnline: false,
}

const PopupReducer: Reducer<Popup, PopupActions | ClearCacheActions> = (stateSlice = emptyPopup, action) => {
	switch (action.type) {
		case SHOW_POPUP:
			return {
				titel: action.titel,
				body: action.body,
				visible: true,
				callback: action.callback,
				cancelCallback: action.cancelCallback,
				akkoordString: action.akkoordString,
				annulerenString: action.annulerenString,
				alleenOnline: action.alleenOnline,
			}
		case AKKOORD_POPUP:
			return {
				...stateSlice,
				...{
					visible: false,
				},
			}
		case CLEAR_POPUP:
			return {
				...stateSlice,
				...{
					visible: false,
				},
			}
		case CLEAR_CACHE:
			return emptyPopup
		default:
			return stateSlice
	}
}

export default PopupReducer