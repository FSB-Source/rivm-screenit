import {getIfExists, getMandatory} from "../util/MapUtil"
import type {AnnotatieAfbeelding} from "../datatypes/AnnotatieAfbeelding"
import {getNextIcoonId, mapAfbeeldingDtoToAfbeelding} from "../datatypes/AnnotatieAfbeelding"
import type {AnnotatieIcoon, AnnotatieIcoonDto} from "../datatypes/AnnotatieIcoon"
import type {VisueleInspectieActions} from "../actions/VisueleInspectieActions"
import {
	DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
	MAAK_VISUELE_INSPECTIE_ICOON,
	MAMMOGRAFIE_OPSLAAN,
	MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG,
	SET_VISUELE_INSPECTIE_AFBEELDING,
	SET_VISUELE_INSPECTIE_ICOON_POSITION,
	SET_VISUELE_INSPECTIE_ICOON_TEKST,
	VERWIJDER_VISUELE_INSPECTIE_ICOON,
	VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
	VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID,
} from "../actions/VisueleInspectieActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const VisueleInspectieIcoonReducer: Reducer<Map<number, AnnotatieAfbeelding>, VisueleInspectieActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, AnnotatieAfbeelding> = new Map()
	const iconenById: Map<number, AnnotatieIcoon> = new Map()

	switch (action.type) {
		case VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID:
			action.visueleInspectieAfbeeldingen.forEach((visueleInspectieAfbeelding: AnnotatieAfbeelding) => {
				result.set(visueleInspectieAfbeelding.afspraakId, visueleInspectieAfbeelding)
			})
			return new Map([...stateSlice, ...result])
		case VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID:
			result.set(action.afspraakId, action.visueleInspectieAfbeelding)
			break
		case SET_VISUELE_INSPECTIE_AFBEELDING:
			result.set(action.afspraakId, mapAfbeeldingDtoToAfbeelding(action.afspraakId, action.visueleInspectieAfbeeldingDto))
			break
		case MAAK_VISUELE_INSPECTIE_ICOON:
			iconenById.set(action.icoonId, {
				icoonId: action.icoonId,
				type: action.icoonType,
				positieX: action.x,
				positieY: action.y,
				nieuwIcoon: true,
			})
			kopieerAfbeelding(action.afspraakId)
			break
		case SET_VISUELE_INSPECTIE_ICOON_POSITION:
			iconenById.set(action.icoonId, {
				...getMandatory(getMandatory(stateSlice, action.afspraakId).iconenById, action.icoonId),
				positieX: action.x,
				positieY: action.y,
			})
			kopieerAfbeelding(action.afspraakId)
			break
		case SET_VISUELE_INSPECTIE_ICOON_TEKST:
			iconenById.set(action.icoonId, {
				...getMandatory(getMandatory(stateSlice, action.afspraakId).iconenById, action.icoonId),
				tekst: action.tekst,
			})
			kopieerAfbeelding(action.afspraakId)
			break
		case VERWIJDER_VISUELE_INSPECTIE_ICOON:
			kopieerAfbeelding(action.afspraakId)
			const afbeelding = getMandatory(result, action.afspraakId)
			afbeelding.iconenById.delete(action.icoonId)
			break
		case MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG:
		case MAMMOGRAFIE_OPSLAAN:
			action.mammografie.visueleInspectieAfbeelding.iconen.forEach((icoon: AnnotatieIcoonDto) => {
				const icoonId: number = getNextIcoonId()
				iconenById.set(icoonId, {
					...icoon,
					icoonId,
					nieuwIcoon: false,
				})
			})
			break
		case DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID:
			stateSlice.delete(action.afspraakId)
			return stateSlice
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}

	return new Map([...stateSlice, ...result])

	function kopieerAfbeelding(afspraakId: number): void {
		const bestaandeAfbeelding = getIfExists(stateSlice, afspraakId)

		if (!bestaandeAfbeelding || !bestaandeAfbeelding.iconenById) {
			result.set(afspraakId, {
				afspraakId: afspraakId,
				iconenById: new Map([...iconenById]),
			})
		} else {
			result.set(afspraakId, {
				afspraakId: afspraakId,
				iconenById: new Map([...bestaandeAfbeelding.iconenById, ...iconenById]),
			})
		}
	}
}

export default VisueleInspectieIcoonReducer