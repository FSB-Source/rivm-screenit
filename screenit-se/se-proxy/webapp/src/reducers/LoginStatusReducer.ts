import type {LoginStatusActions} from "../actions/LoginStatusActions"
import {CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF, SET_LOGIN_ACTIVE, SET_LOGIN_INACTIVE, SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF} from "../actions/LoginStatusActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import type {LoginStatus} from "../datatypes/LoginStatus"
import {Reducer} from "redux"

const initialLoginStatus: LoginStatus = {
	inlogActief: false,
	stopIdentificerenTotYubikeyEraf: false,
}

const LoginStatusReducer: Reducer<LoginStatus, LoginStatusActions | ClearCacheActions> = (stateSlice = initialLoginStatus, action) => {
	switch (action.type) {
		case SET_LOGIN_ACTIVE:
			return {
				...stateSlice,
				inlogActief: true,
			}
		case SET_LOGIN_INACTIVE:
			return {
				...stateSlice,
				inlogActief: false,
			}
		case SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF:
			return {
				...stateSlice,
				stopIdentificerenTotYubikeyEraf: true,
			}
		case CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF:
			return {
				...stateSlice,
				stopIdentificerenTotYubikeyEraf: false,
			}
		case CLEAR_CACHE:
			return initialLoginStatus
		default:
			return stateSlice
	}
}

export default LoginStatusReducer