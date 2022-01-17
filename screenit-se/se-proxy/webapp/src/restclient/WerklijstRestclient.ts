import type {ClientWerklijstItem} from "../datatypes/ClientWerklijstItem"
import {baseUrl, createClientHeaders, fetchApi, fetchApiPromise} from "../util/ApiUtil"
import {showWarningToast, showWijzigingenOpgeslagenToast} from "../util/ToastUtil"
import {store} from "../Store"
import {getMandatory} from "../util/MapUtil"
import {putTransactionToScreenItCentraalPromiseZonderAfspraak} from "./TransactionRestclient"
import {createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch, createActionLogGebeurtenisBeeldenGeenMppsOntvangen} from "../actions/LogGebeurtenisActions"
import type {KwaliteitsopnameScreenITWerklijstItem} from "../datatypes/KwaliteitsopnameScreenITWerklijstItem"
import {aeTitle} from "../util/Util"

export const clientToevoegenAanWerklijst = (clientWerklijstItem: ClientWerklijstItem): void => {
	fetchApi("POST", "werklijst/toevoegen", undefined, JSON.stringify(clientWerklijstItem))
}
export const kwaliteitsopnameToevoegenAanWerklijst = (kwaliteitsopnameScreenITWerklijstItem: KwaliteitsopnameScreenITWerklijstItem): void => {
	fetchApi("POST", "werklijst/startKwaliteitsopname", undefined, JSON.stringify(kwaliteitsopnameScreenITWerklijstItem))
}
export const verwijderVanWerklijst = (aeTitle: string): void => {
	fetchApi("DELETE", `werklijst/verwijder/${aeTitle}`)
}
export const beeindigKwaliteitsopname = (aeTitle: string): void => {
	fetchApi("DELETE", `werklijst/beeindigKwaliteitsopname/${aeTitle}`)
}
export const meldingAfgeslotenProcedure = (clientId: number, uitnodigingsNr: number): void => {
	fetch(`${baseUrl}werklijst/opActieveMppsRecordsLijst/${uitnodigingsNr}`, {
		method: "GET",
		headers: createClientHeaders(),
		body: null,
	}).then(response => {
		if (response.ok) {
			showWarningToast("Controleer of de procedure op de mammograaf is afgesloten. Indien afgesloten, negeer dan deze melding.")
			putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, "LOG_GEBEURTENIS_SE", createActionLogGebeurtenisBeeldenGeenMppsOntvangen()).then(() => {
				showWijzigingenOpgeslagenToast()
			})
		}
	})
}
export const getActieveKwaliteitsopname = (): Promise<KwaliteitsopnameScreenITWerklijstItem> => {
	return fetchApiPromise("GET", `werklijst/actieveKwaliteitsopname/${aeTitle()}`).then(response => {
		return response.json()
	})
}
export const checkOpBeeldenVanAmputatie = (accessionNumber: string, zijde: string | null | undefined): Promise<boolean> => {
	zijde = zijde ? zijde : "GEEN_AMPUTATIE"
	return fetch(`${baseUrl}werklijst/heeftBeeldenZijde/${accessionNumber}/${zijde}`, {
		method: "GET",
		headers: createClientHeaders(),
		body: null,
	}).then(response => {
		return response.ok
	})
}
export const clearWerklijst = (): void => {
	const huidigeMammograafId = store.getState().huidigeMammograafId
	const aeTitle = huidigeMammograafId ? getMandatory(store.getState().mammografenById, huidigeMammograafId).aeTitle : undefined

	if (aeTitle) {
		verwijderVanWerklijst(aeTitle)
	}
}
export const waarschuwingGecontroleerd = (uitnodigingsNr: number, clientId: number): void => {
	putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, "LOG_GEBEURTENIS_SE", createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch()).then(() => {
		showWijzigingenOpgeslagenToast()
	}).then(() => {
		fetch(`${baseUrl}werklijst/waarschuwingGecontroleerd/${uitnodigingsNr}`, {
			method: "POST",
			headers: createClientHeaders(),
			body: null,
		})
	})
}