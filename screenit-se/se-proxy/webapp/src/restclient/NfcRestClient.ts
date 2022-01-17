import {store} from "../Store"
import {fetchWithTimeout} from "../util/FetchUtil"

export type NfcVersieResponse = {
	nfcServerVersie: string;
}

export type NfcOtpResponse = {
	public_id: string;
	otp: string;
}

const fetchTimeoutNfcService = 5000
const nfcBaseUrl = "http:
export const getVersie = (): Promise<NfcVersieResponse> => {
	const environmentInfo = store.getState().environmentInfo
	if (environmentInfo && !environmentInfo.nfcEnabled) {
		return new Promise(resolve => {
			resolve({
				nfcServerVersie: "NFC-disabled",
			})
		})
	}

	return fetchWithTimeout(`${nfcBaseUrl}versie`, fetchTimeoutNfcService, {
		method: "GET",
		mode: "cors",
	}).then((response) => {
		if (response.status === 200) {
			return response.json().then((responseData) => {
				return responseData
			})
		}
	}).catch((error) => {
		console.error(`Communicatiefout met NFC lezer bij getVersie: ${error}`)
	})
}

export const readNFC = (): Promise<NfcOtpResponse> => {
	const environmentInfo = store.getState().environmentInfo
	if (environmentInfo && !environmentInfo.nfcEnabled) {
		return new Promise(resolve => {
			resolve({
				otp: "GEEN_OTP",
				public_id: "GEEN_IDENTIFICATIE",
			})
		})
	}

	return fetchWithTimeout(`${nfcBaseUrl}nfc`, fetchTimeoutNfcService, {
		method: "GET",
		mode: "cors",
	}).then((response) => {
		if (response.status === 200) {
			return response.json().then((responseData) => {
				return responseData
			})
		}
	})
}