import type {ClientActions} from "../actions/ClientActions"
import {SET_EMAILADRES, SET_TELEFOON1, SET_TELEFOON2, SET_TIJDELIJK_ADRES, VUL_CLIENTEN} from "../actions/ClientActions"
import type {Client} from "../datatypes/Client"
import type {AanvullendeInformatieActions} from "../actions/AanvullendeInformatieActions"
import {MAAK_DUBBELE_TIJD, MAAK_DUBBELE_TIJD_REDEN} from "../actions/AanvullendeInformatieActions"
import type {ClearCacheActions} from "../actions/ClearCacheActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import type {PlanningActions} from "../actions/PlanningActions"
import {getMandatory} from "../util/MapUtil"
import {CLIENTGEGEVENS_OPSLAAN, INSCHRIJVEN} from "../actions/AfspraakActions"
import {Reducer} from "redux"

type ClientReducerActions = ClientActions | PlanningActions | AanvullendeInformatieActions | ClearCacheActions;

const ClientenReducer: Reducer<Map<number, Client>, ClientReducerActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, Client> = new Map()

	switch (action.type) {
		case VUL_CLIENTEN:
			action.clienten.forEach((client: Client) => {
				result.set(client.id, client)
			})
			return new Map([...stateSlice, ...result])
		case INSCHRIJVEN:
		case CLIENTGEGEVENS_OPSLAAN:
			if (stateSlice.size === 0) {
				console.warn(`${action.type} ontvangen terwijl de daglijst nog leeg is`)
				return stateSlice
			}

			const clientGegevens: Client = getMandatory(stateSlice, action.clientId)

			if (action.tijdelijkAdres) {
				clientGegevens.tijdelijkAdres = action.tijdelijkAdres
			}

			clientGegevens.telefoonnummer1 = action.telefoonnummer1
			clientGegevens.telefoonnummer2 = action.telefoonnummer2
			result.set(action.clientId, {
				...stateSlice.get(action.clientId),
				...clientGegevens,
			})
			return new Map([...stateSlice, ...result])
		case MAAK_DUBBELE_TIJD:
			const clientDubbeleTijd: Client | void = stateSlice.get(action.clientId)
			if (clientDubbeleTijd) {
				if (action.dubbeleTijd) {
					result.set(action.clientId, {
						...clientDubbeleTijd,
						...{
							doelgroep: "DUBBELE_TIJD",
						},
					})
				} else {
					if (clientDubbeleTijd.doelgroep === "DUBBELE_TIJD") {
						result.set(action.clientId, {
							...clientDubbeleTijd,
							doelgroep: "REGULIER",
						})
					}
				}
			}
			return new Map([...stateSlice, ...result])
		case MAAK_DUBBELE_TIJD_REDEN:
			const clientDubbeleTijdReden: Client | void = stateSlice.get(action.clientId)
			if (clientDubbeleTijdReden) {
				result.set(action.clientId, {
					...clientDubbeleTijdReden,
					...{
						dubbeleTijdReden: action.dubbeleTijdReden,
					},
				})
			}
			return new Map([...stateSlice, ...result])
		case SET_EMAILADRES:
			result.set(action.clientId, {
				...getMandatory(stateSlice, action.clientId),
				emailadres: action.emailadres,
			})
			return new Map([...stateSlice, ...result])
		case SET_TIJDELIJK_ADRES:
			result.set(action.clientId, {
				...getMandatory(stateSlice, action.clientId),
				tijdelijkAdres: action.tijdelijkAdres,
			})
			return new Map([...stateSlice, ...result])
		case SET_TELEFOON1:
			result.set(action.clientId, {
				...getMandatory(stateSlice, action.clientId),
				telefoonnummer1: action.telefoon,
			})
			return new Map([...stateSlice, ...result])
		case SET_TELEFOON2:
			result.set(action.clientId, {
				...getMandatory(stateSlice, action.clientId),
				telefoonnummer2: action.telefoon,
			})
			return new Map([...stateSlice, ...result])
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}
}

export default ClientenReducer