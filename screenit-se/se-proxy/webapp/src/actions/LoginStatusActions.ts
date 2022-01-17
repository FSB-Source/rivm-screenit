export type LoginStatusActions =
	LoginActiveAction
	| LoginInactiveAction
	| StopIdentificerenTotYubikeyErafAction
	| ClearStopIdentificerenTotYubikeyErafAction;
export const SET_LOGIN_ACTIVE = "SET_LOGIN_ACTIVE"
export type LoginActiveAction = {
	type: "SET_LOGIN_ACTIVE";
};
export const createActionLoginActive = (): LoginActiveAction => ({
	type: SET_LOGIN_ACTIVE,
})
export const SET_LOGIN_INACTIVE = "SET_LOGIN_INACTIVE"
export type LoginInactiveAction = {
	type: "SET_LOGIN_INACTIVE";
};
export const createActionLoginInactive = (): LoginInactiveAction => ({
	type: SET_LOGIN_INACTIVE,
})
export const SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF = "SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF"
export type StopIdentificerenTotYubikeyErafAction = {
	type: "SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF";
};
export const createActionStopIdentificerenTotYubikeyEraf = (): StopIdentificerenTotYubikeyErafAction => ({
	type: SET_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF,
})
export const CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF = "CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF"
export type ClearStopIdentificerenTotYubikeyErafAction = {
	type: "CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF";
};
export const createActionClearStopIdentificerenTotYubikeyEraf = (): ClearStopIdentificerenTotYubikeyErafAction => ({
	type: CLEAR_STOP_IDENTIFICEREN_TOT_YUBIKEY_ERAF,
})