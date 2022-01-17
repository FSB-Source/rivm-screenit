import {connect} from "react-redux"
import type {Aanzichtpixels, AnnotatieIcoonViewDispatchProps, AnnotatieIcoonViewStateProps} from "./AnnotatieIcoonView"
import AnnotatieIcoonView from "./AnnotatieIcoonView"
import {
	createActionMaakVisueleInspectieIcoon,
	createActionSetVisueleInspectieIcoonPosition,
	createActionSetVisueleInspectieIcoonTekst,
	createActionVerwijderVisueleInspectieIcoon,
} from "../../../actions/VisueleInspectieActions"
import {getAfbeeldingByType} from "../../../util/IcoonAfbeeldingTypeUtil"
import type {AnnotatieIcoon, AnnotatieIcoonType} from "../../../datatypes/AnnotatieIcoon"
import {
	createActionMaakSignaleringIcoonLinksHorizontaal,
	createActionMaakSignaleringIcoonLinksVerticaal,
	createActionMaakSignaleringIcoonRechtsHorizontaal,
	createActionMaakSignaleringIcoonRechtsVerticaal,
	createActionSetSignaleringIcoonPositionLinksHorizontaal,
	createActionSetSignaleringIcoonPositionLinksVerticaal,
	createActionSetSignaleringIcoonPositionRechtsHorizontaal,
	createActionSetSignaleringIcoonPositionRechtsVerticaal,
	createActionVerwijderSignaleringIcoonLinksHorizontaal,
	createActionVerwijderSignaleringIcoonLinksVerticaal,
	createActionVerwijderSignaleringIcoonRechtsHorizontaal,
	createActionVerwijderSignaleringIcoonRechtsVerticaal,
} from "../../../actions/SignalerenActions"
import {dispatchActions} from "../../../util/DispatchUtil"
import {createActionSetAmputatie} from "../../../actions/AanvullendeInformatieActions"
import {RootState, store} from "../../../Store"
import type {Amputatie} from "../../../datatypes/Onderzoek"
import {showErrorToast} from "../../../util/ToastUtil"
import {verbodenWegensAmputatie, verbodenWegensSignaleringsicoon} from "../OnderzoekContainer"
import {Dispatch} from "redux"

const isVisueleInspectie = (icoonType: AnnotatieIcoonType): boolean => {
	return !(icoonType === "SIGNALERING_MASSA" || icoonType === "SIGNALERING_ARCHITECTUURVERSTORING" || icoonType === "SIGNALERING_ASYMMETRIE" || icoonType === "SIGNALERING_CALCIFICATIES")
}

export type Aanzicht = "RECHTS_VERTICAAL" | "LINKS_VERTICAAL" | "RECHTS_HORIZONTAAL" | "LINKS_HORIZONTAAL";
const amputatieScaleFactor = 3

export interface AnnotatieIcoonContainerDraggableProps extends AnnotatieIcoonContainerSharedProps {
	isDraggable: true;
	afspraakId: number;
}

export interface AnnotatieIcoonContainerNotDraggableProps extends AnnotatieIcoonContainerSharedProps {
	isDraggable: false;
}

export interface AnnotatieIcoonContainerSharedProps {
	icoon: AnnotatieIcoon;
	imageWidth: number;
	imageHeight: number;
	isOpPalet?: boolean;
	aanzicht?: Aanzicht;
	aanzichten?: Map<Aanzicht, Aanzichtpixels>;
	metTextbox: boolean;
}

export type AnnotatieIcoonContainerProps = {
	afspraakId?: number;
	icoon: AnnotatieIcoon;
	imageWidth: number;
	imageHeight: number;
	isOpPalet?: boolean;
	aanzicht?: Aanzicht;
	aanzichten?: Map<Aanzicht, Aanzichtpixels>;
	metTextbox: boolean;
	isDraggable?: boolean;
}

const mapStateToProps = (state: RootState, ownProps: AnnotatieIcoonContainerProps): AnnotatieIcoonViewStateProps => {
	if (!ownProps.afspraakId && ownProps.isDraggable) {
		console.warn("Let op! afspraakId moet gevuld zijn bij isDraggable")
	}

	const icoonWidth = getAfbeeldingByType(ownProps.icoon.type).width
	const icoonHeight = getAfbeeldingByType(ownProps.icoon.type).height
	const isNietVisueleInspectie = state.navigation.subPagina !== "Visuele inspectie"

	return {
		...ownProps,
		icoon: ownProps.icoon,
		icoonWidth: icoonWidth,
		icoonHeight: icoonHeight,
		amputatieSize: ownProps.imageWidth / amputatieScaleFactor,
		isNietVisueleInspectie: isNietVisueleInspectie,
	}
}

const mapDispatchToProps = (dispatch: Dispatch, ownProps: AnnotatieIcoonContainerProps): AnnotatieIcoonViewDispatchProps => ({
	verwijderIcoon(afspraakId: number, icoonId: number, x: number, y: number, aanzicht: Aanzicht | undefined): void {
		if (isVisueleInspectie(ownProps.icoon.type)) {
			if (ownProps.icoon.type === "AMPUTATIE") {
				dispatchActions(dispatch, createActionSetAmputatie(afspraakId, undefined))
			} else {
				dispatchActions(dispatch, createActionVerwijderVisueleInspectieIcoon(afspraakId, icoonId))
			}
		} else {
			switch (aanzicht) {
				case "RECHTS_VERTICAAL":
					dispatchActions(dispatch, createActionVerwijderSignaleringIcoonRechtsVerticaal(afspraakId, icoonId))
					break
				case "LINKS_VERTICAAL":
					dispatchActions(dispatch, createActionVerwijderSignaleringIcoonLinksVerticaal(afspraakId, icoonId))
					break
				case "RECHTS_HORIZONTAAL":
					dispatchActions(dispatch, createActionVerwijderSignaleringIcoonRechtsHorizontaal(afspraakId, icoonId))
					break
				case "LINKS_HORIZONTAAL":
					dispatchActions(dispatch, createActionVerwijderSignaleringIcoonLinksHorizontaal(afspraakId, icoonId))
					break
				default:
					console.error(`onbekend aanzicht: ${aanzicht}`)
			}
		}
	},
	setPosition(afspraakId: number, icoonId: number, x: number, y: number, aanzicht: Aanzicht | undefined): void {
		if (isVisueleInspectie(ownProps.icoon.type)) {
			const amputatie = x2amputatie(x)
			if (ownProps.icoon.type === "AMPUTATIE") {
				if (!verbodenWegensSignaleringsicoon(afspraakId, amputatie)) {
					setAnnotatieIndienMogelijk(afspraakId, amputatie, dispatch)
				}
			} else {
				if (!verbodenWegensAmputatie(afspraakId, amputatie)) {
					dispatchActions(dispatch, createActionSetVisueleInspectieIcoonPosition(afspraakId, icoonId, x, y))
				}
			}
		} else {
			switch (aanzicht) {
				case "RECHTS_VERTICAAL":
					if (!verbodenWegensAmputatie(afspraakId, "RECHTERBORST")) {
						dispatchActions(dispatch, createActionSetSignaleringIcoonPositionRechtsVerticaal(afspraakId, icoonId, x, y))
					}
					break
				case "LINKS_VERTICAAL":
					if (!verbodenWegensAmputatie(afspraakId, "LINKERBORST")) {
						dispatchActions(dispatch, createActionSetSignaleringIcoonPositionLinksVerticaal(afspraakId, icoonId, x, y))
					}
					break
				case "RECHTS_HORIZONTAAL":
					if (!verbodenWegensAmputatie(afspraakId, "RECHTERBORST")) {
						dispatchActions(dispatch, createActionSetSignaleringIcoonPositionRechtsHorizontaal(afspraakId, icoonId, x, y))
					}
					break
				case "LINKS_HORIZONTAAL":
					if (!verbodenWegensAmputatie(afspraakId, "LINKERBORST")) {
						dispatchActions(dispatch, createActionSetSignaleringIcoonPositionLinksHorizontaal(afspraakId, icoonId, x, y))
					}
					break
				default:
					console.error(`onbekend aanzicht: ${aanzicht}`)
			}
		}
	},
	maakIcoon(afspraakId: number, x: number, y: number, type: AnnotatieIcoonType, aanzicht: Aanzicht | undefined): void {
		if (isVisueleInspectie(ownProps.icoon.type)) {
			const amputatie = x2amputatie(x)
			if (ownProps.icoon.type === "AMPUTATIE") {
				if (!verbodenWegensSignaleringsicoon(afspraakId, amputatie)) {
					setAnnotatieIndienMogelijk(afspraakId, amputatie, dispatch)
				}
			} else {
				if (!verbodenWegensAmputatie(afspraakId, amputatie)) {
					dispatchActions(dispatch, createActionMaakVisueleInspectieIcoon(afspraakId, type, x, y))
				}
			}
		} else {
			switch (aanzicht) {
				case "RECHTS_VERTICAAL":
					if (!verbodenWegensAmputatie(afspraakId, "RECHTERBORST")) {
						dispatchActions(dispatch, createActionMaakSignaleringIcoonRechtsVerticaal(afspraakId, type, x, y))
					}
					break
				case "LINKS_VERTICAAL":
					if (!verbodenWegensAmputatie(afspraakId, "LINKERBORST")) {
						dispatchActions(dispatch, createActionMaakSignaleringIcoonLinksVerticaal(afspraakId, type, x, y))
					}
					break
				case "RECHTS_HORIZONTAAL":
					if (!verbodenWegensAmputatie(afspraakId, "RECHTERBORST")) {
						dispatchActions(dispatch, createActionMaakSignaleringIcoonRechtsHorizontaal(afspraakId, type, x, y))
					}
					break
				case "LINKS_HORIZONTAAL":
					if (!verbodenWegensAmputatie(afspraakId, "LINKERBORST")) {
						dispatchActions(dispatch, createActionMaakSignaleringIcoonLinksHorizontaal(afspraakId, type, x, y))
					}
					break
				default:
					console.error(`onbekend aanzicht: ${aanzicht}`)
			}
		}
	},
	verwerkTextChange(afspraakId: number, icoonId: number, newValue: string): void {
		dispatchActions(dispatch, createActionSetVisueleInspectieIcoonTekst(afspraakId, icoonId, newValue))
	},
})

function heeftAnnotatieIcoon(afspraakId: number, amputatie: Amputatie): AnnotatieIcoon | undefined {
	const annotatieAfbeelding = store.getState().visueleInspectieAfbeeldingByAfspraakId.get(afspraakId)

	if (!annotatieAfbeelding || !annotatieAfbeelding.iconenById) {
		return undefined
	}

	for (const icoon of annotatieAfbeelding.iconenById.values()) {
		if (x2amputatie(icoon.positieX) === amputatie) {
			return icoon
		}
	}

	return undefined
}

function setAnnotatieIndienMogelijk(afspraakId: number, amputatie: Amputatie, dispatch: Dispatch): void {
	const annotatieIcoon = heeftAnnotatieIcoon(afspraakId, amputatie)

	if (annotatieIcoon) {
		showErrorToast(`De ${amputatie.toLowerCase()} heeft een "${annotatieIcoon.type.toLowerCase().replace("_", " ")}" icoon. Verwijder dit icoon voordat je de ${amputatie.toLowerCase()} als geamputeerd markeert.`)
	} else {
		dispatchActions(dispatch, createActionSetAmputatie(afspraakId, amputatie))
	}
}

function x2amputatie(x: number): Amputatie {
	return x < 50 ? "RECHTERBORST" : "LINKERBORST"
}

const AnnotatieIcoonContainer = connect(mapStateToProps, mapDispatchToProps)(AnnotatieIcoonView)
export default AnnotatieIcoonContainer