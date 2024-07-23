import {UPDATE_FORM_FIELD, UpdateFormFieldAction} from "../actions/FormActions"
import {KIES_GEEN_HUISARTS_OPTIE, KIES_HUISARTS} from "../actions/HuisartsActions"
import {BEZWAAR_AANVRAGEN} from "../actions/AfspraakActions"
import {WIJZIGINGEN_GEMAAKT, WIJZIGINGEN_VERWERKT} from "../actions/WijzigingenActions"
import {
	MAAK_AANVULLENDE_INFORMATIE_OPERATIE,
	MAAK_EXTRA_MEDEWERKER,
	MAAK_MBB_OPMERKING,
	MAAK_RADIOLOOG_OPMERKING,
	MAAK_REDEN_FOTOBESPREKING,
	MAAK_SUBOPTIMALE_INSTELTECHNIEK,
	OPERATIE_LINKS,
	OPERATIE_RECHTS,
} from "../actions/MBBSignaleringActions"
import {
	MAAK_VISUELE_INSPECTIE_ICOON,
	SET_VISUELE_INSPECTIE_ICOON_POSITION,
	SET_VISUELE_INSPECTIE_ICOON_TEKST,
	VERWIJDER_VISUELE_INSPECTIE_ICOON,
} from "../actions/VisueleInspectieActions"
import {
	MAAK_ADVIES_HUISARTS,
	MAAK_EERDER_MAMMOGRAM_JAARTAL,
	MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING,
	MAAK_EXTRA_FOTOS_REDENEN,
	MAAK_ONDERBROKEN_ONDERZOEK,
	MAAK_ONDERZOEK_TYPE,
	MAAK_ONVOLLEDIG_ONDERZOEK,
	SET_AMPUTATIE,
} from "../actions/AanvullendeInformatieActions"
import {
	MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL,
	MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL,
	MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL,
	MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL,
	SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL,
	SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL,
	SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL,
	SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL,
	UPDATE_HEEFT_AFWIJKINGEN,
	VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL,
	VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL,
	VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL,
	VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL,
} from "../actions/SignalerenActions"
import {SET_TIJDELIJK_ADRES} from "../actions/ClientActions"
import {CLEAR_CACHE} from "../actions/ClearCacheActions"
import {Reducer, UnknownAction} from "redux"

let laatsteIndentificatie: string | undefined

const WijzigingenReducer: Reducer<boolean> = (stateSlice = false, action: UnknownAction): boolean => {
	switch (action.type) {
		case WIJZIGINGEN_GEMAAKT:
			const identificatieNrElement = document.getElementById("Identificatienummer")
			const modalShowingElements = document.getElementsByClassName("show")

			if (identificatieNrElement && modalShowingElements.length === 0) {
				identificatieNrElement.focus()
			}

			return stateSlice
		case UPDATE_FORM_FIELD:
			if (action.formId === "passant_afspraak_maken") {
				return stateSlice
			}

			const updateFormFieldAction = action as UpdateFormFieldAction
			if (action.formField && updateFormFieldAction.formField.label && updateFormFieldAction.formField.value?.identificatienummer) {
				if (updateFormFieldAction.formField.label === "Identificatienummer") {
					if (updateFormFieldAction.formField.value.identificatienummer === laatsteIndentificatie) {
						return stateSlice
					}

					laatsteIndentificatie = updateFormFieldAction.formField.value.identificatienummer
				}
			}
			return true
		case KIES_HUISARTS:
		case KIES_GEEN_HUISARTS_OPTIE:
		case BEZWAAR_AANVRAGEN:
		case SET_TIJDELIJK_ADRES:
		case MAAK_AANVULLENDE_INFORMATIE_OPERATIE:
		case MAAK_REDEN_FOTOBESPREKING:
		case MAAK_EXTRA_MEDEWERKER:
		case MAAK_MBB_OPMERKING:
		case MAAK_RADIOLOOG_OPMERKING:
		case MAAK_SUBOPTIMALE_INSTELTECHNIEK:
		case OPERATIE_RECHTS:
		case OPERATIE_LINKS:
		case MAAK_VISUELE_INSPECTIE_ICOON:
		case SET_VISUELE_INSPECTIE_ICOON_POSITION:
		case SET_VISUELE_INSPECTIE_ICOON_TEKST:
		case VERWIJDER_VISUELE_INSPECTIE_ICOON:
		case SET_AMPUTATIE:
		case MAAK_EERDER_MAMMOGRAM_JAARTAL:
		case MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING:
		case MAAK_ONVOLLEDIG_ONDERZOEK:
		case MAAK_ONDERBROKEN_ONDERZOEK:
		case MAAK_EXTRA_FOTOS_REDENEN:
		case MAAK_ONDERZOEK_TYPE:
		case MAAK_ADVIES_HUISARTS:
		case UPDATE_HEEFT_AFWIJKINGEN:
		case MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
		case MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
		case MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL:
		case MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL:
		case SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL:
		case SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL:
		case SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL:
		case SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL:
		case VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL:
		case VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL:
		case VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
		case VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
			return true
		case WIJZIGINGEN_VERWERKT:
		case CLEAR_CACHE:
			return false
		default:
			return stateSlice
	}
}

export default WijzigingenReducer