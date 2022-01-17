import type {SignalerenActions} from "../actions/SignalerenActions"
import {
	MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL,
	MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL,
	MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL,
	MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL,
	SET_SIGNALERING,
	SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL,
	SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL,
	SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL,
	SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL,
	UPDATE_HEEFT_AFWIJKINGEN,
	VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL,
	VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL,
	VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL,
	VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL,
	VUL_SIGNALERING_BY_AFSPRAAK_ID,
} from "../actions/SignalerenActions"
import type {Signalering} from "../datatypes/Signalering"
import {mapSignaleringFromDto} from "../datatypes/Signalering"
import {getIfExists, getMandatory} from "../util/MapUtil"
import type {AnnotatieAfbeelding} from "../datatypes/AnnotatieAfbeelding"
import type {AnnotatieIcoon} from "../datatypes/AnnotatieIcoon"
import type {AfspraakDto} from "../datatypes/Afspraak"
import type {DoorsnedeAfbeeldingen} from "../datatypes/DoorsnedeAfbeeldingen"
import {ONDERZOEK_OPSLAAN, OnderzoekActions} from "../actions/OnderzoekActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const emptyAfbeelding = (afspraakId: number): AnnotatieAfbeelding => {
	return {
		afspraakId: afspraakId,
		iconenById: new Map(),
	}
}

const SignalerenReducer: Reducer<Map<number, any>, SignalerenActions | OnderzoekActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<number, any> = new Map()
	const iconenById: Map<number, AnnotatieIcoon> = new Map()

	switch (action.type) {
		case CLEAR_CACHE:
			return new Map()
		case VUL_SIGNALERING_BY_AFSPRAAK_ID:
			action.afspraken.forEach((afspraak: AfspraakDto) => {
				const signalering = afspraak.signaleren ? mapSignaleringFromDto(afspraak.signaleren, afspraak.id) : undefined
				result.set(afspraak.id, signalering)
			})
			break
		case UPDATE_HEEFT_AFWIJKINGEN:
		case MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
		case SET_SIGNALERING:
		case MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
		case MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL:
		case MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL:
		case SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL:
		case SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL:
		case SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL:
		case SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL:
		case VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
		case VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
		case VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL:
		case VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL:
		case ONDERZOEK_OPSLAAN:
			const bestaandeSignalering: Signalering | null = getIfExists(stateSlice, action.afspraakId)
			switch (action.type) {
				case UPDATE_HEEFT_AFWIJKINGEN:
					const signalering: Signalering | null = getIfExists(stateSlice, action.afspraakId)
					result.set(action.afspraakId, {
						...signalering,
						...{
							heeftAfwijkingen: action.heeftAfwijkingen,
							doorsnedeAfbeeldingen: {
								rechtsVerticaleDoorsnede: null,
								linksVerticaleDoorsnede: null,
								rechtsHorizontaleDoorsnede: null,
								linksHorizontaleDoorsnede: null,
							},
						},
					})
					break
				case MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
					iconenById.set(action.icoonId, getIcoon(action))
					if (bestaandeSignalering) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newRechtsHorizontaleDoorsnede: AnnotatieAfbeelding = updateAanzicht(bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action, iconenById)
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsHorizontaleDoorsnede: {
									...newRechtsHorizontaleDoorsnede,
								},
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case SET_SIGNALERING:
					result.set(action.afspraakId, mapSignaleringFromDto(action.signaleringDto, action.afspraakId))
					break
				case MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
					iconenById.set(action.icoonId, getIcoon(action))
					if (bestaandeSignalering) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newLinksHorizontaleDoorsnede: AnnotatieAfbeelding = updateAanzicht(bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action, iconenById)
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksHorizontaleDoorsnede: {
									...newLinksHorizontaleDoorsnede,
								},
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL:
					iconenById.set(action.icoonId, getIcoon(action))
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newRechtsVerticaleDoorsnede: AnnotatieAfbeelding = updateAanzicht(bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede : emptyAfbeelding(action.afspraakId), action, iconenById)
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsVerticaleDoorsnede: {
									...newRechtsVerticaleDoorsnede,
								},
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL:
					iconenById.set(action.icoonId, getIcoon(action))
					if (bestaandeSignalering) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newLinksVerticaleDoorsnede: AnnotatieAfbeelding = updateAanzicht(bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede : emptyAfbeelding(action.afspraakId), action, iconenById)
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksVerticaleDoorsnede: {
									...newLinksVerticaleDoorsnede,
								},
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksHorizontaleDoorsnede: getUpdatedAnnotatieAfbeeldingMoveIcoon(bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsHorizontaleDoorsnede: getUpdatedAnnotatieAfbeeldingMoveIcoon(bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksVerticaleDoorsnede: getUpdatedAnnotatieAfbeeldingMoveIcoon(bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsVerticaleDoorsnede: getUpdatedAnnotatieAfbeeldingMoveIcoon(bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksHorizontaleDoorsnede: getUpdatedAnnotatieAfbeeldingDeleteIcoon(bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsHorizontaleDoorsnede: getUpdatedAnnotatieAfbeeldingDeleteIcoon(bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL:
					if (bestaandeSignalering && bestaandeSignalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede) {
						const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								linksVerticaleDoorsnede: getUpdatedAnnotatieAfbeeldingDeleteIcoon(bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede ? bestaandeDoorsnedeAfbeeldingen.linksVerticaleDoorsnede : emptyAfbeelding(action.afspraakId), action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL:
					const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = getMandatory(stateSlice, action.afspraakId).doorsnedeAfbeeldingen
					if (bestaandeSignalering && bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede) {
						const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
							...bestaandeDoorsnedeAfbeeldingen,
							...{
								rechtsVerticaleDoorsnede: getUpdatedAnnotatieAfbeeldingDeleteIcoon(bestaandeDoorsnedeAfbeeldingen.rechtsVerticaleDoorsnede, action),
							},
						}
						result.set(action.afspraakId, {
							...bestaandeSignalering,
							...{
								doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
							},
						})
					}
					break
				case ONDERZOEK_OPSLAAN:
					if (action.onderzoek.amputatie === "LINKERBORST") {
						const bestaandeSignalering: Signalering | null = getIfExists(stateSlice, action.afspraakId)

						if (bestaandeSignalering) {
							const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = bestaandeSignalering.doorsnedeAfbeeldingen
							const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
								...bestaandeDoorsnedeAfbeeldingen,
								...{
									linksHorizontaleDoorsnede: emptyAfbeelding(action.afspraakId),
									linksVerticaleDoorsnede: emptyAfbeelding(action.afspraakId),
								},
							}
							result.set(action.afspraakId, {
								...bestaandeSignalering,
								...{
									doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
								},
							})
						}
					} else if (action.onderzoek.amputatie === "RECHTERBORST") {
						const bestaandeSignalering: Signalering | null = getIfExists(stateSlice, action.afspraakId)

						if (bestaandeSignalering) {
							const bestaandeDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = bestaandeSignalering.doorsnedeAfbeeldingen
							const newDoorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = {
								...bestaandeDoorsnedeAfbeeldingen,
								...{
									rechtsHorizontaleDoorsnede: emptyAfbeelding(action.afspraakId),
									rechtsVerticaleDoorsnede: emptyAfbeelding(action.afspraakId),
								},
							}
							result.set(action.afspraakId, {
								...bestaandeSignalering,
								...{
									doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen,
								},
							})
						}
					}
					break
			}
			break
		default:
			return stateSlice
	}

	return new Map([...stateSlice, ...result])
}

const updateAanzicht = (doorsnede: AnnotatieAfbeelding, action: any, iconenById: Map<number, AnnotatieIcoon>): AnnotatieAfbeelding => {
	if (!doorsnede.iconenById) {
		doorsnede = {
			afspraakId: action.afspraakId,
			iconenById: new Map(),
		}
	}

	return {
		afspraakId: action.afspraakId,
		iconenById: new Map([...doorsnede.iconenById ? doorsnede.iconenById : new Map(), ...iconenById]),
	}
}

const getUpdatedAnnotatieAfbeeldingMoveIcoon = (bestaandeDoorsnedeAfbeelding: AnnotatieAfbeelding, action: any): AnnotatieAfbeelding => {
	const result: AnnotatieAfbeelding = {
		afspraakId: action.afspraakId,
		iconenById: new Map(),
	}
	const newIconenById: Map<number, AnnotatieIcoon> = new Map()

	if (bestaandeDoorsnedeAfbeelding && bestaandeDoorsnedeAfbeelding.iconenById && bestaandeDoorsnedeAfbeelding.iconenById.has(action.icoonId)) {
		newIconenById.set(action.icoonId, {
			...getMandatory(bestaandeDoorsnedeAfbeelding.iconenById, action.icoonId),
			...{
				positieX: action.x,
				positieY: action.y,
			},
		})
		return {
			afspraakId: action.afspraakId,
			iconenById: new Map([...bestaandeDoorsnedeAfbeelding.iconenById, ...newIconenById]),
		}
	}

	return result
}

const getUpdatedAnnotatieAfbeeldingDeleteIcoon = (bestaandeDoorsnedeAfbeelding: AnnotatieAfbeelding, action: any): AnnotatieAfbeelding => {
	const newIconenById: Map<number, AnnotatieIcoon> = new Map([...bestaandeDoorsnedeAfbeelding.iconenById ? bestaandeDoorsnedeAfbeelding.iconenById : new Map()])
	newIconenById.delete(action.icoonId)
	return {
		afspraakId: action.afspraakId,
		iconenById: new Map([...newIconenById]),
	}
}

const getIcoon = (action: any): AnnotatieIcoon => {
	return {
		icoonId: action.icoonId,
		type: action.icoonType,
		positieX: action.x,
		positieY: action.y,
		tekst: action.tekst,
		nieuwIcoon: false,
	}
}

export default SignalerenReducer