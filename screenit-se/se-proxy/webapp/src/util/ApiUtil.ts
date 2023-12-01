import {store} from "../Store"
import {createActionFatalError} from "../actions/ErrorActions"
import {MELDING_SESSIE_NIET_GELDIG, MELDING_TECHNISCHE_FOUT, showErrorToast, showErrorToastWithoutAutoClose} from "./ToastUtil"
import {logoutClient} from "../restclient/AuthenticatieRestclient"
import type {ErrorDto} from "../datatypes/ErrorDto"
import {datumFormaat, nu} from "./DateUtil"

export const baseUrl = "./api/"
export const fetchApi = (method: string, url: string, callBack?: ((...args: Array<any>) => any), body?: string): void => {
	fetch(baseUrl + url, {
		method: method,
		headers: createClientHeaders(),
		body: body,
	}).then((response: any) => {
		const session = store.getState().session
		if (response.status === 403) {
			console.log(`Uitloggen door HTTP status 403: ${getYubikeyIdentificatie() || "onbekend"}`)
			logoutClient()
		} else if (!response.ok) {
			if (!response.data) {
				console.warn(`Geen response data bij api call met status ${response.status}: ${baseUrl}${url}:${method}`)
				const errorVanSe: ErrorDto = {
					errorReferentie: `${session ? session.seCode : "(Geen sessie)"}-${datumFormaat(String(nu()))}`,
				}
				store.dispatch(createActionFatalError(errorVanSe))
			} else {
				response.json().then((error: ErrorDto) => {
					if (error && error.errorReferentie) {
						console.warn(error.errorReferentie)
						store.dispatch(createActionFatalError(error))
					} else {
						const errorVanSe: ErrorDto = {
							errorReferentie: `${session ? session.seCode : "(Geen sessie)"}-${datumFormaat(String(nu()))}`,
						}
						console.warn(errorVanSe)
						store.dispatch(createActionFatalError(errorVanSe))
					}
				})
			}
		} else if (callBack) {
			return response.json().then((result: any) => {
				if (callBack) {
					try {
						callBack(result)
					} catch (error) {
						showErrorToast(MELDING_TECHNISCHE_FOUT)
						console.log(`Error: ${error}`)
					}
				}
			})
		}
	}).catch(errorResponse => {
		console.log(errorResponse.toString())
	})
}

export const fetchApiPromise = (method: string, url: string, body?: string): Promise<any> => {
	return new Promise((resolve, reject) => {
		fetch(baseUrl + url, {
			method: method,
			headers: createClientHeaders(),
			body: body,
		}).then(response => {
			if (response.status === 403) {
				console.log(`Uitloggen door HTTP status 403: ${getYubikeyIdentificatie() || "onbekend"}`)
				logoutClient()
				showErrorToastWithoutAutoClose(MELDING_SESSIE_NIET_GELDIG)
				reject("Not authorized")
			} else if (response.status === 500) {
				response.json().then(error => {
					console.log(error.errorReferentie)
					store.dispatch(createActionFatalError(error))
				}).catch(error => {
					showErrorToast(MELDING_TECHNISCHE_FOUT)
					console.log(`Error response: ${error}`)
				})
				reject(response)
			} else {
				return resolve(response)
			}
		}).catch(errorResponse => {
			reject(errorResponse)
		})
	})
}

function getYubikeyIdentificatie(): string | undefined {
	return store.getState().session?.yubikeyIdentificatie
}

function getAccountId(): number | undefined {
	return store.getState().session?.instellingGebruikerId
}

export const createClientHeaders = (): Headers => {
	const clientHeaders = new Headers()
	clientHeaders.append("Accept", "application/json, text/plain, */*")
	clientHeaders.append("Content-Type", "application/json")
	const yubikeyIdentificatie = getYubikeyIdentificatie()
	if (yubikeyIdentificatie) {
		clientHeaders.append("YubikeyIdentificatie", yubikeyIdentificatie)
	}
	const accountId = getAccountId()
	if (accountId) {
		clientHeaders.append("accountId", String(accountId))
	}
	return clientHeaders
}