import React from "react"

export type PopupActions = ShowPopupAction | AkkoordPopupAction | ClearPopupAction;
export const SHOW_POPUP = "SHOW_POPUP"
export type ShowPopupAction = {
	type: "SHOW_POPUP";
	titel: string;
	body: React.ReactNode;
	callback?: () => void;
	cancelCallback?: (() => void);
	akkoordString?: string;
	annulerenString?: string;
	alleenOnline: boolean;
};
export const createActionShowPopup = (titel: string, body: React.ReactNode, callback?: () => void, cancelCallback?: (() => void), akkoordString?: string, annulerenString?: string, alleenOnline?: boolean): ShowPopupAction => {
	return {
		type: SHOW_POPUP,
		titel: titel,
		body: body,
		callback: callback,
		cancelCallback: cancelCallback,
		akkoordString: akkoordString,
		annulerenString: annulerenString,
		alleenOnline: alleenOnline ?? false,
	}
}

export const AKKOORD_POPUP = "AKKOORD_POPUP"
export type AkkoordPopupAction = {
	type: "AKKOORD_POPUP";
};
export const createActionAkkoordPopup = (): AkkoordPopupAction => {
	return {
		type: AKKOORD_POPUP,
	}
}

export const CLEAR_POPUP = "CLEAR_POPUP"
export type ClearPopupAction = {
	type: "CLEAR_POPUP";
};
export const createActionClearPopup = (): ClearPopupAction => {
	return {
		type: CLEAR_POPUP,
	}
}