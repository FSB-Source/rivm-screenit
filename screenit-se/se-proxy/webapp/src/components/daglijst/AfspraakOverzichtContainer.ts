import {connect} from "react-redux"
import type {Client} from "../../datatypes/Client"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import {Tijdslot} from "../../datatypes/Planning"
import {Afspraak} from "../../datatypes/Afspraak"
import type {Onderzoekstatus} from "../../datatypes/Onderzoek"
import AfspraakOverzichtView, {AfspraakOverzichtViewProps} from "./AfspraakOverzichtView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): AfspraakOverzichtViewProps => {
	const planning = getIfExists(state.planning, state.daglijstDatum)
	const huidigeDagAfspraken: Array<Afspraak> = [...state.afsprakenById.values()].filter((afspraak: Afspraak) => afspraak.vanafDatum === state.daglijstDatum)
	const geplandeAfspraken: Array<Afspraak> = huidigeDagAfspraken.filter((afspraak: Afspraak) => afspraak.status !== "BEEINDIGD")
	const nietAfgerondeTijdSlots: Array<Tijdslot> = []
	nietAfgerondeTijdSlots.push(...geplandeAfspraken)

	if (planning) {
		nietAfgerondeTijdSlots.push(...planning.geenScreeningBlokken.filter(geenScreeningblok => new Date(geenScreeningblok.totDatumTijd) >= new Date()))
	}

	const sortedNietAfgerondeTijdSlots: Array<Tijdslot> = nietAfgerondeTijdSlots.sort((a: Tijdslot, b: Tijdslot) => {
		if (a.vanafTijd > b.vanafTijd) {
			return 1
		} else if (a.vanafTijd < b.vanafTijd) {
			return -1
		}

		if (a instanceof Afspraak && b instanceof Afspraak) {
			const clientA: Client = getMandatory(state.clientenById, a.clientId)
			const clientB: Client = getMandatory(state.clientenById, b.clientId)
			return clientA.bsn > clientB.bsn ? 1 : -1
		}

		return 0
	})
	const sortedAfgerondeTijdSlots: Array<Tijdslot> = []
	sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter((afspraak: Afspraak) => afspraak.status === "BEEINDIGD" && heeftAfgerondStatus(state, afspraak.id, "ONDERBROKEN")).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1))
	sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter((afspraak: Afspraak) => afspraak.status === "BEEINDIGD" && heeftAfgerondStatus(state, afspraak.id, "ONVOLLEDIG")).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1))
	sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter((afspraak: Afspraak) => afspraak.status === "BEEINDIGD" && heeftAfgerondStatus(state, afspraak.id, "AFGEROND")).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1))

	if (planning) {
		sortedAfgerondeTijdSlots.push(...planning.geenScreeningBlokken.filter(geenScreeningblok => new Date(geenScreeningblok.totDatumTijd) < new Date()).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1))
	}

	return {
		nietAfgerondeTijdSlots: sortedNietAfgerondeTijdSlots,
		afgerondeTijdSlots: sortedAfgerondeTijdSlots,
		clienten: state.clientenById,
		daglijstDatum: state.daglijstDatum,
	}
}

const heeftAfgerondStatus = (state: RootState, afspraakId: number, status: Onderzoekstatus): boolean => {
	const onderzoek = getIfExists(state.onderzoekByAfspraakId, afspraakId)
	return !!(onderzoek && onderzoek.status === status)
}

const AfspraakOverzichtContainer = connect(mapStateToProps)(AfspraakOverzichtView)
export default AfspraakOverzichtContainer