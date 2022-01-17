import {Afspraak} from "../datatypes/Afspraak"
import {store} from "../Store"
import type {Onderzoek} from "../datatypes/Onderzoek"

export const getDagAfspraken = (datum: string): Array<Afspraak> => {
	return [...store.getState().afsprakenById.values()].filter((afspraak: Afspraak) => afspraak.vanafDatum === datum)
}

export const getOnderzoekStatusCount = (status: string, onderzoeken: Map<number, Onderzoek>, afspraken: Afspraak[]): number => {
	let totalResult = 0
	afspraken.forEach((afspraak) => {
		if (status === "AFGEROND" || status === "ONDERBROKEN" || status === "ONVOLLEDIG") {
			if (afspraak.status === "BEEINDIGD") {
				const onderzoek = onderzoeken.get(afspraak.id)

				if (onderzoek !== undefined && onderzoek.status === status) {
					totalResult += 1
				}
			}
		} else {
			if (afspraak.status === status) {
				totalResult += 1
			}
		}
	})
	return totalResult
}

export const hasOpenstaandeOnderzoeken = (datum: string): boolean => {
	return getDagAfspraken(datum).some((afspraak) => {
		return afspraak.status === "INGESCHREVEN" || afspraak.status === "ONDERZOEK" || afspraak.status === "SIGNALEREN"
	})
}

export const hasNietDoorgevoerdeOnderzoeken = (datum: string): boolean => {
	const dagverslag = store.getState().dagverslag

	if (!dagverslag || !dagverslag.has(datum)) {
		return false
	}

	return getDagAfspraken(datum).some((afspraak) => {
		return afspraak.status === "BEEINDIGD" && !afspraak.doorgevoerd
	})
}