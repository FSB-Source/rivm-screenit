import {createActionClearSession, createActionSetSession} from "../actions/SessionActions"
import {store} from "../Store"
import {leesAfspraken} from "./DaglijstRestclient"
import {createActionKiesDaglijstDatum} from "../actions/DaglijstDatumActions"
import {nuTimestamp, vandaagISO} from "../util/DateUtil"
import {baseUrl, createClientHeaders} from "../util/ApiUtil"
import {dismissAllToasts, showErrorToast, showErrorToastWithoutAutoClose, showYubikeyHerkendToast} from "../util/ToastUtil"
import {createActionSetAutorisatie} from "../actions/AutorisatieActions"
import {leesPlanning} from "./PlanningRestClient"
import {leesHuisartsen} from "./HuisartsenRestclient"
import {logoffToIMS, logonToIMS} from "../util/ImsApiUtil"
import {readMammografen} from "./MammografenRestClient"
import {leesZorginstellingen} from "./ZorginstellingenRestclient"
import {ensureIdleCheck} from "../util/TimeoutUtil"
import {readSeGebruikers} from "./SeGebruikerRestClient"
import {clearWerklijst, getActieveKwaliteitsopname} from "./WerklijstRestclient"
import {createActionNavigateToDaglijst, createActionNavigateToKwaliteitsopname, NAVIGATE_TO_KWALITEITSOPNAME, NavigationActions} from "../actions/NavigationActions"
import {createActionClearCache} from "../actions/ClearCacheActions"
import {getVersie} from "./NfcRestClient"
import {aeTitle} from "../util/Util"
import {dispatchActions} from "../util/DispatchUtil"
import {createActionLoginActive, createActionLoginInactive, createActionStopIdentificerenTotYubikeyEraf} from "../actions/LoginStatusActions"
import {readMammografenStatus} from "./MammografenStatusRestclient"
import {isAuthorized} from "../util/AutorisatieUtil"
import {AutorisatieDto} from "../datatypes/AutorisatieDto"
import {navigateToConnectiestatus} from "../util/NavigationUtil"

export const GEEN_OTP = "GEEN_OTP"
export const GEEN_IDENTIFICATIE = "GEEN_IDENTIFICATIE"
export const YUBIKEY_AFWEZIG = "YUBIKEY_AFWEZIG"
export const YUBIKEY_ONBEKEND = "YUBIKEY_ONBEKEND"
export const YUBIKEY_HERKEND = "YUBIKEY_HERKEND"
export const YUBIKEY_VERLOPEN = "YUBIKEY_VERLOPEN"

export const identificeer = (yubikeyIdentificatie: string, yubikey: string): void => {
	if (store.getState().loginStatus.inlogActief) {
		console.log(`${nuTimestamp()}: Skip identificeren omdat er login bezig is`)
		return
	}

	fetch(`${baseUrl}authenticatie/identificeren`, {
		method: "POST",
		body: yubikeyIdentificatie,
	}).then((response) => {
		if (response.status === 200) {
			response.json().then(identificatie => {
				verwerkSuccesIdentificeerResponse(identificatie, yubikeyIdentificatie, yubikey)
			})
		}
	}).catch((error) => {
		showErrorToast("Inloggen mislukt.")
		console.error(`Er ontstond een probleem tijdens het uitvoeren van fetch bij identificeren: ${error.message}`)
	})
}

function verwerkSuccesIdentificeerResponse(identificatie: any, yubikeyIdentificatie: string, yubikey: string): void {
	if (identificatie.code === YUBIKEY_AFWEZIG) {
		dismissAllToasts()
	} else if (identificatie.code === YUBIKEY_ONBEKEND || identificatie.code === YUBIKEY_VERLOPEN) {
		if (!store.getState().loginStatus.inlogActief) {
			showYubikeyHerkendToast()
		}
	} else if (identificatie.code === YUBIKEY_HERKEND) {
		if (store.getState().session === null) {
			dismissAllToasts()
			login("", "", yubikeyIdentificatie, yubikey)
		}
	}
}

export const login = (username: string, password: string, yubikeyIdentificatie: string, yubikey: string): void => {
	if (store.getState().loginStatus.inlogActief) {
		console.log(`${nuTimestamp()}: Skip inlogpoging omdat er al een bezig is`)
		return
	}

	dispatchActions(store.dispatch, createActionLoginActive())
	getVersie().then((versieResponse) => {
		const credentials = Buffer.from(`${username}:${password}`).toString("base64")
		const loginHeader = new Headers({
			Authorization: `Basic ${credentials}`,
			yubikeyIdentificatie: yubikeyIdentificatie,
			Yubikey: yubikey ? yubikey : GEEN_OTP,
			nfcServerVersie: versieResponse.nfcServerVersie,
		})
		const sessieMeenemen: boolean = username === "" && password === ""
		const meldingTechnischeFout = "Inloggen mislukt (technische fout)"
		fetch(`${baseUrl}authenticatie/inloggen`, {
			method: "POST",
			headers: loginHeader,
		}).then((loginResponse) => {
			verwerkLoginResponse(loginResponse, yubikeyIdentificatie, sessieMeenemen, meldingTechnischeFout)
		}).catch((error) => {
			mislukteLoginAfronden(meldingTechnischeFout, `(authenticatieMislukt): ${error}`, sessieMeenemen, yubikeyIdentificatie)
		})
	})
}

function verwerkLoginResponse(loginResponse: Response, yubikeyIdentificatie: string, sessieMeenemen: boolean, meldingTechnischeFout: string): void {
	if (loginResponse.status === 200) {
		verwerkSuccesvolleLogin(loginResponse, yubikeyIdentificatie).then(() => {
			console.log(`${nuTimestamp()}: Klaar met inloggen${sessieMeenemen ? " (sessiemeenemen)" : ""}yubikey: ${yubikeyIdentificatie}`)
			dispatchActions(store.dispatch, createActionLoginInactive())
		}).catch((error) => {
			mislukteLoginAfronden(meldingTechnischeFout, `(error in verwerkSuccesvolleLogin): ${error}`, sessieMeenemen, yubikeyIdentificatie)
		})
	} else if (loginResponse.status === 412) {
		mislukteLoginAfronden("SE is niet bekend in ScreenIT, neem contact op met een beheerder.", "(SE onbekend)", sessieMeenemen, yubikeyIdentificatie)
	} else if (loginResponse.status === 504) {
		mislukteLoginAfronden("SE is offline, alleen bestaande sessies meenemen mogelijk", "(SE offline)", sessieMeenemen, yubikeyIdentificatie)
	} else {
		loginResponse.json().then(error => {
			const uiBericht = error && error.message ? error.message : "Fout bij inloggen, neem contact op met een beheerder"
			mislukteLoginAfronden(uiBericht, `(Overige error): ${uiBericht}`, sessieMeenemen, yubikeyIdentificatie)
		}).catch((error) => {
			mislukteLoginAfronden(meldingTechnischeFout, `(error tijdens parse json bij error resonse): ${error}`, sessieMeenemen, yubikeyIdentificatie)
		})
	}
}

function verwerkSuccesvolleLogin(response: any, yubikeyIdentificatie: string): Promise<any> {
	const token: string = response.headers.get("x-auth-token")
	let autorisatie: AutorisatieDto

	if (!token) {
		showErrorToast("Er ging iets fout tijdens het inloggen, probeer het opnieuw of neem contact op met een beheerder")
		console.error("Fout tijdens inloggen: Geen token.")
		return new Promise<void>(resolve => resolve())
	}

	return response.json().then((autorisatieObject: AutorisatieDto) => {
		ensureIdleCheck()
		autorisatie = autorisatieObject
		const navigatieActie = JSON.parse(autorisatie.navigatie)
		store.dispatch(createActionSetSession(token, autorisatie.username, autorisatie.medewerkercode, autorisatie.displayName, autorisatie.seCode, autorisatie.seNaam,
			yubikeyIdentificatie, autorisatie.instellingGebruikerId))
		store.dispatch(createActionSetAutorisatie(autorisatie))
		return navigatieActie
	}).then((navigatieActie: NavigationActions) => {
		store.dispatch(createActionKiesDaglijstDatum(vandaagISO()))
		readMammografen().then(mammografen => {
			if (magAlleenConnectieStatusInzien(autorisatie)) {
				readMammografenStatus(mammografen).then(() => afrondenSuccesvolleLogin(autorisatie, navigatieActie))
				return
			} else if (aeTitle() !== "") {
				inloggenOpStationMetMammograaf(autorisatie, navigatieActie)
			} else {
				if (navigatieActie.type === NAVIGATE_TO_KWALITEITSOPNAME) {
					navigatieActie = createActionNavigateToDaglijst()
				}
				afrondenSuccesvolleLogin(autorisatie, navigatieActie)
			}

			readMammografenStatus(mammografen)
		}).catch(() => {
			afrondenSuccesvolleLogin(autorisatie, navigatieActie)
		})

		if (!magAlleenConnectieStatusInzien(autorisatie)) {
			leesHuisartsen()
			leesPlanning(vandaagISO())
			leesZorginstellingen()
			readSeGebruikers()
		}
	})
}

function inloggenOpStationMetMammograaf(autorisatie: AutorisatieDto, navigatieActie: NavigationActions): void {
	getActieveKwaliteitsopname().then(kwaliteitsopname => {
		if (kwaliteitsopname.aeTitle) {
			navigatieActie = createActionNavigateToKwaliteitsopname()
		}
		afrondenSuccesvolleLogin(autorisatie, navigatieActie)
	}).catch(() => {
		afrondenSuccesvolleLogin(autorisatie, navigatieActie)
	})
}

const afrondenSuccesvolleLogin = (autorisatie: AutorisatieDto, navigatieActie: NavigationActions): void => {
	if (magAlleenConnectieStatusInzien(autorisatie)) {
		navigateToConnectiestatus(store.dispatch)
	} else {
		if (Object.keys(navigatieActie).length === 0) {
			navigatieActie = createActionNavigateToDaglijst()
		}
		logonToIMS(autorisatie.username)
		leesAfspraken(vandaagISO(), navigatieActie)
	}
}

const mislukteLoginAfronden = (uiBericht: string, logBericht: string, sessieMeenemen: boolean, yubikeyIdentificatie: string): void => {
	let extraLogtekst = ""

	if (sessieMeenemen) {
		extraLogtekst = "(sessiemeenemen) "
		dispatchActions(store.dispatch, createActionStopIdentificerenTotYubikeyEraf())
		console.log(`Stop identificeren totdat Yubikey verwijderd wordt: ${yubikeyIdentificatie}`)
		const uiTekst = `Fout tijdens meenemen sessie:\n${uiBericht}`
		showErrorToastWithoutAutoClose(uiTekst)
	} else {
		showErrorToast(uiBericht)
	}

	console.log(`${nuTimestamp()}: Klaar met inloggen ${extraLogtekst}voor yubikey: ${yubikeyIdentificatie} ${logBericht}`)
	dispatchActions(store.dispatch, createActionLoginInactive())
}

export const logout = (yubikeyIdentificatie?: string): void => {
	if (!yubikeyIdentificatie) {
		logoutClient()
	} else {
		fetch(`${baseUrl}authenticatie/uitloggen`, {
			method: "POST",
			headers: createClientHeaders(),
			body: yubikeyIdentificatie,
		}).then(() => {
			logoutClient()
		}).catch((error) => {
			logoutClient()
			showErrorToast("Uitloggen mislukt.")
			console.error(`Er ontstond een probleem tijdens het uitvoeren van fetch bij uitloggen: ${error.message}`)
		})
	}
}

export const logoutClient = (): void => {
	clearWerklijst()

	const session = store.getState().session
	if (session) {
		logoffToIMS(session.gebruikersnaam)
	} else {
		console.warn("Logout terwijl er geen sessie is.")
	}

	store.dispatch(createActionClearSession())
	store.dispatch(createActionClearCache())
}

const magAlleenConnectieStatusInzien = (autorisatie: AutorisatieDto): boolean => {
	return !isAuthorized(autorisatie.inschrijvenRecht) && !isAuthorized(autorisatie.onderzoekenRecht) && !isAuthorized(autorisatie.signalerenRecht) && !isAuthorized(autorisatie.kwaliteitsopnameRecht) && isAuthorized(autorisatie.connectiestatusRecht)
}
