import {store} from "../Store"
import {readNFC} from "../restclient/NfcRestClient"
import {GEEN_IDENTIFICATIE, identificeer, logoutClient} from "../restclient/AuthenticatieRestclient"
import {dismissAllToasts, dismissNfcErrorToast, dismissYubikeyHerkendToast, showNfcErrorToast} from "./ToastUtil"
import {dispatchActions} from "./DispatchUtil"
import {createActionClearStopIdentificerenTotYubikeyEraf} from "../actions/LoginStatusActions"

const NFC_POLL_INTERVAL = 1000
export const AANTAL_INTERVALLEN_VOOR_LOGOUT = 2
let vorigePublicId: string | undefined

export const startPollingNfc = (resterendeIntervallenVoorLogout: number): void => {
	window.setTimeout(() => {
		if (store.getState().dubbeleInstantie) {
			console.warn("Dubbele ScreenIT instantie gedetecteerd op werkstation: stop pollen NFC ")
			return
		}
		leesEnVerwerkNfcStatus(resterendeIntervallenVoorLogout)
	}, NFC_POLL_INTERVAL)
}

function leesEnVerwerkNfcStatus(resterendeIntervallenVoorLogout: number): void {
	readNFC().then((responseData) => {
		const publicId = responseData.public_id
		const otp = responseData.otp
		let volgendeResterendeIntervallen = AANTAL_INTERVALLEN_VOOR_LOGOUT
		dismissNfcErrorToast()

		if (store.getState().loginStatus.stopIdentificerenTotYubikeyEraf && publicId !== vorigePublicId) {
			dispatchActions(store.dispatch, createActionClearStopIdentificerenTotYubikeyEraf())
			dismissAllToasts()
			console.log(`Herstart identificeren door verwijderen Yubikey ${vorigePublicId} huidige yubikey: ${publicId}`)
		}

		const ingelogdePublicId = getIngelogdePublicId()

		if (ingelogdePublicId) {
			if (publicId === GEEN_IDENTIFICATIE) {
				if (resterendeIntervallenVoorLogout <= 0) {
					console.log(`Uitloggen door verwijderen Yubikey: ${ingelogdePublicId}`)
					logoutClient()
				} else {
					volgendeResterendeIntervallen = resterendeIntervallenVoorLogout - 1
				}
			} else {
				if (publicId !== ingelogdePublicId) {
					console.log(`Uitloggen ${ingelogdePublicId} door nieuwe Yubikey: ${publicId}`)
					logoutClient()
				} else {
				}
			}
		} else if (publicId !== GEEN_IDENTIFICATIE && publicId !== undefined) {
			if (!store.getState().loginStatus.stopIdentificerenTotYubikeyEraf) {
				identificeer(publicId, otp)
			}
		} else if (publicId === GEEN_IDENTIFICATIE) {
			dismissYubikeyHerkendToast()
		} else {
			console.error(`Onverwachte public-id van NFC server: ${publicId}`)
		}

		vorigePublicId = publicId
		startPollingNfc(volgendeResterendeIntervallen)
	}).catch((error: any) => {
		nfcServiceCommunicatiefout(error)
	})
}

function nfcServiceCommunicatiefout(error: Error): void {
	const ingelogdePublicId = getIngelogdePublicId()

	if (ingelogdePublicId) {
		console.log(`Uitloggen van Yubikey: ${ingelogdePublicId} door communicatiefout met NFC lezer: ${error}`)
		logoutClient()
	} else {
		console.log(`Communicatiefout met NFC lezer in uitgelogde toestand: ${error}`)
	}

	showNfcErrorToast()
	startPollingNfc(AANTAL_INTERVALLEN_VOOR_LOGOUT)
}

function getIngelogdePublicId(): string | undefined {
	return store.getState().session?.yubikeyIdentificatie
}