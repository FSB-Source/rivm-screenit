import type {AfspraakActions, ClearAfsprakenAction} from "../actions/AfspraakActions"
import {
	AFSPRAAK_AFRONDEN,
	AFSPRAAK_DOORVOEREN,
	AFSPRAAK_SIGNALEREN,
	BEZWAAR_AANVRAGEN,
	CLEAR_AFSPRAKEN,
	CLIENTGEGEVENS_OPSLAAN,
	INSCHRIJVEN,
	KIES_IDENTIFICATIENUMMER,
	KIES_IDENTIFICATIESOORT,
	UITSCHRIJVEN,
	VUL_AFSPRAKEN,
} from "../actions/AfspraakActions"
import {Afspraak} from "../datatypes/Afspraak"
import {KIES_GEEN_HUISARTS_OPTIE, KIES_HUISARTS} from "../actions/HuisartsActions"
import {ONDERZOEK_STARTEN, OnderzoekActions} from "../actions/OnderzoekActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

type AfspraakMap = Map<number, Afspraak>;

const AfsprakenReducer: Reducer<AfspraakMap, AfspraakActions | OnderzoekActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, Afspraak> = new Map()

	switch (action.type) {
		case CLEAR_CACHE:
			return new Map()
		case VUL_AFSPRAKEN:
			if (action.afspraken) {
				const afspraken = ((action.afspraken as any) as Array<Afspraak>)

				for (const afspraak of afspraken) {
					result.set(afspraak.id, afspraak)
				}
			}
			break
		case CLEAR_AFSPRAKEN:
			const clearAction: ClearAfsprakenAction = ((action as any) as ClearAfsprakenAction)

			for (const afspraak of stateSlice.values()) {
				if (afspraak.vanafDatum !== clearAction.datum) {
					result.set(afspraak.id, afspraak)
				}
			}
			return result
		case INSCHRIJVEN:
		case CLIENTGEGEVENS_OPSLAAN:
		case UITSCHRIJVEN:
		case ONDERZOEK_STARTEN:
		case AFSPRAAK_SIGNALEREN:
		case AFSPRAAK_AFRONDEN:
		case KIES_IDENTIFICATIESOORT:
		case KIES_IDENTIFICATIENUMMER:
		case BEZWAAR_AANVRAGEN:
		case KIES_HUISARTS:
		case KIES_GEEN_HUISARTS_OPTIE:
		case AFSPRAAK_DOORVOEREN:
			const afspraak: Afspraak | void = stateSlice.get(action.afspraakId)
			if (afspraak) {
				const copyAfspraak: Afspraak = Object.create(afspraak)
				switch (action.type) {
					case INSCHRIJVEN:
						copyAfspraak.status = "INGESCHREVEN"
						copyAfspraak.bezwaarAangevraagd = action.bezwaarAangevraagd
						copyAfspraak.geenHuisartsOptie = action.geenHuisartsOptie
						copyAfspraak.huisartsId = action.huisartsId
						copyAfspraak.identificatienummer = action.identificatienummer
						copyAfspraak.identificatiesoort = action.identificatiesoort
						break
					case CLIENTGEGEVENS_OPSLAAN:
						copyAfspraak.bezwaarAangevraagd = action.bezwaarAangevraagd
						copyAfspraak.geenHuisartsOptie = action.geenHuisartsOptie
						copyAfspraak.huisartsId = action.huisartsId
						copyAfspraak.identificatienummer = action.identificatienummer
						copyAfspraak.identificatiesoort = action.identificatiesoort
						break
					case UITSCHRIJVEN:
						copyAfspraak.status = "VERWACHT"
						break
					case ONDERZOEK_STARTEN:
						copyAfspraak.status = "ONDERZOEK"
						break
					case AFSPRAAK_SIGNALEREN:
						copyAfspraak.status = "SIGNALEREN"
						break
					case AFSPRAAK_AFRONDEN:
						copyAfspraak.status = "BEEINDIGD"
						break
					case KIES_IDENTIFICATIESOORT:
						copyAfspraak.identificatiesoort = action.identificatiesoort
						break
					case KIES_IDENTIFICATIENUMMER:
						copyAfspraak.identificatienummer = action.identificatienummer
						break
					case BEZWAAR_AANVRAGEN:
						copyAfspraak.bezwaarAangevraagd = action.bezwaarAangevraagd
						break
					case KIES_HUISARTS:
						copyAfspraak.huisartsId = action.huisartsId
						copyAfspraak.geenHuisartsOptie = undefined
						break
					case KIES_GEEN_HUISARTS_OPTIE:
						copyAfspraak.geenHuisartsOptie = action.geenHuisartsOptie
						copyAfspraak.huisartsId = undefined
						break
					case AFSPRAAK_DOORVOEREN:
						copyAfspraak.doorgevoerd = true
						break
				}
				result.set(action.afspraakId, copyAfspraak)
			}
			break
	}

	return new Map([...stateSlice, ...result])
}

export default AfsprakenReducer