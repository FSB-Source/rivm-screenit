import React from "react"
import {fetchApiPromise} from "../util/ApiUtil"
import type {ClientDto} from "../datatypes/Client"
import type {PassantDto} from "../datatypes/Passant"
import {createActionAfspraakMakenPassant} from "../actions/PassantAfspraakMakenActions"
import {dispatchActions} from "../util/DispatchUtil"
import {createActionShowPopup} from "../actions/PopupActions"
import {showErrorToast} from "../util/ToastUtil"
import {datumFormaat, vandaagISO} from "../util/DateUtil"
import {putTransactionToScreenItCentraalPromiseZonderAfspraak} from "./TransactionRestclient"
import {createActionNavigateToClientgegevens} from "../actions/NavigationActions"
import type {ErrorDto} from "../datatypes/ErrorDto"
import {newPassantAfspraakMakenForm} from "../components/daglijst/PassantAfspraakMakenContainer"
import {createActionUpdateForm} from "../actions/FormActions"
import PassantPopupView from "../components/melding/PassantPopupView"
import type {AfspraakDto} from "../datatypes/Afspraak"
import {vulAfspraken} from "./DaglijstRestclient"
import {createActionKiesDaglijstDatum} from "../actions/DaglijstDatumActions"
import {store} from "../Store"
import {Dispatch} from "redux"

export const readPassant = (bsn: string, geboortedatum: string, dispatch: any): void => {
	fetchApiPromise("GET", `passanten/?bsn=${bsn}&geboortedatum=${geboortedatum}`).then(response => {
		if (response.status === 404) {
			response.json().then((err: ErrorDto) => {
				showErrorToast(err.errorReferentie)
			})
		}

		if (response.ok) {
			return response.json()
		}

		return undefined
	}).then((passant?: PassantDto) => {
		dispatchPassant(dispatch, passant)
	})
}

function dispatchPassant(dispatch: Dispatch, passant?: PassantDto): void {
	if (passant && passant.clientSeDto) {
		dispatchActions(dispatch, createActionShowPopup("Afspraak maken cliënt", <PassantPopupView
			naam={`${passant.clientSeDto.voorletters} ${passant.clientSeDto.aanspreekTussenvoegselEnAchternaam}`}
			bsn={passant.clientSeDto.bsn} geboortedatum={datumFormaat(passant.clientSeDto.geboortedatum)}
			afspraakVanaf={datumFormaat(passant.afspraakVanaf)} afspraakSe={passant.afspraakSe}
			uitnodigingsDatum={datumFormaat(passant.uitnodigingsDatum)}
			eenmaligeAfmelding={passant.eenmaligeAfmelding}/>, () => {
			afterAkkoord(dispatch, passant.clientSeDto)
		}, () => {
			dispatchActions(dispatch, createActionUpdateForm("passant_afspraak_maken", newPassantAfspraakMakenForm()))
		}, "Akkoord", "Annuleren", true))
	}
}

function afterAkkoord(dispatch: Dispatch, client?: ClientDto): void {
	if (client) {
		const clientId: number = client.id
		const inschrijvenPassantAction = createActionAfspraakMakenPassant(client.geboortedatum, client.bsn)
		putTransactionToScreenItCentraalPromiseZonderAfspraak(client.id, undefined, "INSCHRIJVEN_PASSANT", inschrijvenPassantAction).then(() => {
			window.setTimeout(() => {
				return fetchApiPromise("GET", `daglijst/geforceerd/${vandaagISO()}`).then(response => {
					if (response && response.ok) {
						return response.json()
					}

					return null
				}).then(daglijstMetMutaties => {
					if (daglijstMetMutaties) {
						const afspraakDtos = JSON.parse(daglijstMetMutaties.daglijstJson)
						store.dispatch(createActionKiesDaglijstDatum(vandaagISO()))
						const afspraak: AfspraakDto = afspraakDtos.find((afspraakDto: AfspraakDto) => {
							return afspraakDto.client.id === clientId
						})
						vulAfspraken(vandaagISO(), createActionNavigateToClientgegevens(clientId, afspraak.id), afspraakDtos, [])
					}
				}).catch(error => console.log(error))
			}, 1000)
		})
	}
}
