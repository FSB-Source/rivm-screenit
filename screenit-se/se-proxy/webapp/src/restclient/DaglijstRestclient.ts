import {store} from "../Store"
import {createActionClearAfspraken, createActionVulAfspraken} from "../actions/AfspraakActions"
import {createActionVulClienten} from "../actions/ClientActions"
import type {AfspraakDto} from "../datatypes/Afspraak"
import {Afspraak} from "../datatypes/Afspraak"
import type {Client, ClientDto} from "../datatypes/Client"
import {mapClientDtoToClient} from "../datatypes/Client"
import {fetchApi} from "../util/ApiUtil"
import {createActionVulOnderzoekByAfspraakId} from "../actions/OnderzoekActions"
import type {AnnotatieAfbeelding} from "../datatypes/AnnotatieAfbeelding"
import {mapAfbeeldingDtoToAfbeelding} from "../datatypes/AnnotatieAfbeelding"
import type {MammografieDto} from "../datatypes/Mammografie"
import {createActionVulVisueleInspectieAfbeeldingenByAfspraakId} from "../actions/VisueleInspectieActions"
import {createActionVulSignaleringByAfspraakId} from "../actions/SignalerenActions"
import {createActionNavigateToDaglijst, NavigationActions} from "../actions/NavigationActions"
import {dismissAllToasts, showErrorToast} from "../util/ToastUtil"
import {leesPlanning} from "./PlanningRestClient"
import {vandaagISO} from "../util/DateUtil"
import {getDagverslag} from "./DagverslagRestClient"
import type {Transaction} from "../datatypes/Transaction"
import {dispatchActions} from "../util/DispatchUtil"
import {createActionDaglijstOpgehaald} from "../actions/OpgehaaldeDagenActions"
import {restoreNavigation} from "../util/NavigationUtil"

const clientFromDto = (clientDto: ClientDto): Client => {
	return mapClientDtoToClient(clientDto)
}

const afspraakFromDto = (afspraakDto: AfspraakDto): Afspraak => {
	const afspraak: Afspraak = new Afspraak(
		afspraakDto.id,
		afspraakDto.vanaf.split("T")[0],
		afspraakDto.vanaf.split("T")[1].slice(0, 5),
		afspraakDto.client.id,
		afspraakDto.status,
		afspraakDto.uitnodigingsNr,
		afspraakDto.aantalOproepen,
		afspraakDto.aantalOpgekomen,
		afspraakDto.bezwaarAangevraagd,
		afspraakDto.bezwaarAangevraagd,
		afspraakDto.doorgevoerd,
		afspraakDto.centralAvailable,
		afspraakDto.eerderOnderbrokenInZelfdeRonde,
	)
	afspraak.onderzoekId = afspraakDto.huidigOnderzoek?.id
	afspraak.identificatiesoort = afspraakDto.identificatiesoort
	afspraak.identificatienummer = afspraakDto.identificatienummer
	afspraak.huisartsId = afspraakDto.huisartsId
	afspraak.geenHuisartsOptie = afspraakDto.geenHuisartsOptie
	afspraak.eerdereOpschortenReden = afspraakDto.eerdereOpschortenReden
	afspraak.eerdereOpschortenRedenTekst = afspraakDto.eerdereOpschortenRedenTekst
	afspraak.geforceerd = afspraakDto.geforceerd

	return afspraak
}

const visueleInspectieAfbeeldingFromDto = (afspraakId: number, mammografieDto: MammografieDto): AnnotatieAfbeelding => {
	return mapAfbeeldingDtoToAfbeelding(afspraakId, mammografieDto.visueleInspectieAfbeelding)
}

export const GEFORCEERD_DAGLIJST_OPHALEN = true
export const leesAfspraken = (datum: string, navigatieActie: NavigationActions | undefined, geforceerd = false): void => {
	fetchApi("GET", `daglijst/${geforceerd ? "geforceerd/" : ""}${datum}`, daglijstMetMutaties => {
		vulAfspraken(datum, navigatieActie, JSON.parse(daglijstMetMutaties.daglijstJson), daglijstMetMutaties.mutatieJsons)
		dispatchActions(store.dispatch, createActionDaglijstOpgehaald(datum))
	})

	if (store.getState().online) {
		getDagverslag(datum)
	}
}

export const vulAfspraken = (datum: string, navigatieActie: NavigationActions | undefined, afspraakDtos: Array<AfspraakDto>, mutatiesJson: Array<string>): void => {
	if (!afspraakDtos || (afspraakDtos as any).errorReferentie) {
		showErrorToast("Daglijst en/of dagverslag niet beschikbaar.")
		return
	}

	new Promise<void>((resolve) => {
		store.dispatch(createActionClearAfspraken(datum))
		store.dispatch(createActionVulClienten(afspraakDtos.map((afspraakDto: AfspraakDto) => clientFromDto(afspraakDto.client))))
		store.dispatch(createActionVulAfspraken(afspraakDtos.map((afspraakDto: AfspraakDto) => afspraakFromDto(afspraakDto))))
		store.dispatch(createActionVulVisueleInspectieAfbeeldingenByAfspraakId(afspraakDtos.filter((afspraakDto: AfspraakDto) => afspraakDto.mammografie && afspraakDto.mammografie.visueleInspectieAfbeelding).map((afspraakDto: AfspraakDto) => visueleInspectieAfbeeldingFromDto(afspraakDto.id, afspraakDto.mammografie))))
		store.dispatch(createActionVulOnderzoekByAfspraakId(afspraakDtos))
		store.dispatch(createActionVulSignaleringByAfspraakId(afspraakDtos))
		mutatiesJson.forEach(mutatie => {
			const receivedTransaction: Transaction = JSON.parse(mutatie)
			dispatchActions(store.dispatch, ...receivedTransaction.actions)
		})
		dismissAllToasts()
		resolve()
	}).then(() => {
		if (navigatieActie && navigatieActie.type && !isEmpty(navigatieActie)) {
			restoreNavigation(store.dispatch, navigatieActie)
		}
	})
}

export const vernieuwAfsprakenDaglijst = (geforceerd = false): void => {
	store.dispatch(createActionClearAfspraken(vandaagISO()))
	const leesGeforceerd: boolean = geforceerd && store.getState().online
	leesAfspraken(vandaagISO(), createActionNavigateToDaglijst(), leesGeforceerd)
	leesPlanning(vandaagISO())
}

function isEmpty(obj: any): boolean {
	for (const key in obj) {
		if (Object.prototype.hasOwnProperty.call(obj, key)) {
			return false
		}
	}
	return true
}
