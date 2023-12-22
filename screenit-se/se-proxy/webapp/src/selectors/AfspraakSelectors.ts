import {RootState} from "../Store"
import {Afspraak, Afspraakstatus} from "../datatypes/Afspraak"
import {
	getAantalAfsprakenUitDagverslag,
	getAantalBeeindigdeAfsprakenUitDagverslag,
	getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag,
	getAantalOnderzoekenMetStatusUitDagverslag,
} from "./DagverslagSelector"
import {Onderzoekstatus} from "../datatypes/Onderzoek"
import {heeftAfspraakOnderzoekStatus} from "./OnderzoekSelector"

export const getDagAfspraken = (state: RootState): Array<Afspraak> => {
	return [...state.afsprakenById.values()].filter((afspraak) => afspraak.vanafDatum === state.daglijstDatum)
}

export const getAantalNietBeeindigdeAfsprakenMetStatus = (state: RootState, status: Exclude<Afspraakstatus, "BEEINDIGD" | "KWALITEITSOPNAME">): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken.filter((afspraak) => afspraak.status === status).length
	}
	return getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag(state, status)
}

export const getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus = (state: RootState, status: Exclude<Onderzoekstatus, "ACTIEF">): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken
			.filter((afspraak) => afspraak.status === "BEEINDIGD"
				&& heeftAfspraakOnderzoekStatus(state, afspraak, status)).length
	}
	return getAantalOnderzoekenMetStatusUitDagverslag(state, status)
}

export const getTotaalAfsprakenAfgerondDag = (state: RootState): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken.filter((afspraak) => afspraak.status === "BEEINDIGD").length
	}
	return getAantalBeeindigdeAfsprakenUitDagverslag(state)
}

export const getTotaalAfsprakenDag = (state: RootState): number => {
	const dagAfspraken = getDagAfspraken(state)
	return dagAfspraken.length !== 0 ? dagAfspraken.length : getAantalAfsprakenUitDagverslag(state)
}