import {RootState} from "../Store"
import {Afspraakstatus} from "../datatypes/Afspraak"
import {Onderzoekstatus} from "../datatypes/Onderzoek"
import {DagPlanningSamenvatting} from "../datatypes/Dagverslag"

export const getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag = (state: RootState, status: Exclude<Afspraakstatus, "KWALITEITSOPNAME" | "BEEINDIGD">): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	return dagSamenvatting && status === "VERWACHT" ? dagSamenvatting.aantalVerwacht : 0
}

export const getAantalOnderzoekenMetStatusUitDagverslag = (state: RootState, status: Exclude<Onderzoekstatus, "ACTIEF">): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		switch (status) {
			case "ONDERBROKEN":
				return dagSamenvatting.aantalOnderbroken
			case "ONVOLLEDIG":
				return dagSamenvatting.aantalOnvolledig
			case "AFGEROND":
				return dagSamenvatting.aantalAfgerond
		}
	}
	return 0
}

export const getAantalBeeindigdeAfsprakenUitDagverslag = (state: RootState): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		return dagSamenvatting.aantalAfgerond + dagSamenvatting.aantalOnvolledig + dagSamenvatting.aantalOnderbroken
	}
	return 0
}

export const getAantalAfsprakenUitDagverslag = (state: RootState): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		return dagSamenvatting.aantalVerwacht + dagSamenvatting.aantalAfgerond + dagSamenvatting.aantalOnvolledig + dagSamenvatting.aantalOnderbroken
	}
	return 0
}

const getDagSamenvattingVanDag = (state: RootState): DagPlanningSamenvatting | undefined => {
	return state.dagverslag.get(state.daglijstDatum)?.dagPlanningSamenvatting
}