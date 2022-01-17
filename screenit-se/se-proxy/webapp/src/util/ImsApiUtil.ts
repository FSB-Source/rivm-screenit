import {createEmptyStudyIms, createLogoffIms, createLogonIms, createStudyIms} from "./ImsFactory"
import {store} from "../Store"
import {createActionSetStudyForIms} from "../actions/ImsActions"
import {createActionPutIMSConnectieStatus} from "../actions/ConnectieStatusActions"

const url = "https:

const getStandardHeaders = (): Headers => {
	const headers = new Headers()
	headers.append("Content-type", "application/json; charset=utf-8")
	return headers
}

export const setStudyForIms = (uitnodigingsNr?: number): void => {
	store.dispatch(createActionSetStudyForIms(uitnodigingsNr))
}

export const sendStudyMessageToIMS = (uitnodigingsNr: number, bsn: string, username: string): void => {
	console.log("Study openen naar IMS")
	setStudyForIms(uitnodigingsNr)
	fetchIMS(createStudyIms(uitnodigingsNr, bsn, username))
}

export const sendEmptyStudyMessageToIMS = (username: string): void => {
	console.log("Empty study naar IMS")
	setStudyForIms(undefined)
	fetchIMS(createEmptyStudyIms(username))
}

export const logonToIMS = (username: string): void => {
	console.log("Inloggen naar IMS")
	setStudyForIms(undefined)
	fetchIMS(createLogonIms(username))
}

export const logoffToIMS = (username: string): void => {
	console.log("Uitloggen naar IMS")
	setStudyForIms(undefined)
	fetchIMS(createLogoffIms(username))
}

const fetchIMS = (imsAction: any): void => {
	fetch(url, {
		method: "PUT",
		mode: "cors",
		headers: getStandardHeaders(),
		body: JSON.stringify(imsAction),
	}).then(() => {
		store.dispatch(createActionPutIMSConnectieStatus("OK"))
	}).catch(() => {
		store.dispatch(createActionPutIMSConnectieStatus("FAULT"))
	})
}