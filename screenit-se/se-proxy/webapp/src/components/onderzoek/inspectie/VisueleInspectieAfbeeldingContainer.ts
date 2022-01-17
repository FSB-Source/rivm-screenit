import {connect} from "react-redux"
import VisueleInspectieAfbeeldingView, {VisueleInspectieAfbeeldingViewProps} from "./VisueleInspectieAfbeeldingView"
import {getIfExists, getMandatory} from "../../../util/MapUtil"
import {iconenLijst} from "../../../datatypes/IcoonAfbeelding"
import * as CoordinationCalculator from "../../../util/CoordinatenCalculator"
import {originalImageScaleFactor} from "../../../util/CoordinatenCalculator"
import {createActionVulVisueleInspectieAfbeeldingByAfspraakId} from "../../../actions/VisueleInspectieActions"
import {dispatchActions} from "../../../util/DispatchUtil"
import {RootState, store} from "../../../Store"

export type VisueleInspectieAfbeeldingContainerProps = {
	containerId?: string;
	width: number;
	height?: number;
	afterLoad?: () => void;
	isEditable?: boolean;
	noGutters?: boolean;
};

const mapStateToProps = (state: RootState, ownProps: VisueleInspectieAfbeeldingContainerProps): VisueleInspectieAfbeeldingViewProps => {
	let visueleInspectieAfbeelding = getIfExists(state.visueleInspectieAfbeeldingByAfspraakId, state.navigation.afspraakId)
	const client = getMandatory(state.clientenById, state.navigation.clientId)
	const afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)

	if (!visueleInspectieAfbeelding) {
		if (client.vorigeOnderzoeken && client.vorigeOnderzoeken[0]) {
			dispatchActions(store.dispatch, createActionVulVisueleInspectieAfbeeldingByAfspraakId(afspraak.id, client.vorigeOnderzoeken[0].visueleInspectieAfbeelding))
			visueleInspectieAfbeelding = client.vorigeOnderzoeken[0].visueleInspectieAfbeelding
		} else {
			visueleInspectieAfbeelding = {
				afspraakId: state.navigation.afspraakId || 0,
				iconenById: new Map(),
			}
		}
	}

	return {
		containerId: ownProps.containerId,
		width: ownProps.width || CoordinationCalculator.getWidth(`visuele-inspectie-afbeelding${ownProps.containerId}`),
		height: ownProps.height || ownProps.width * originalImageScaleFactor,
		afspraakId: afspraak.id,
		iconenById: visueleInspectieAfbeelding.iconenById,
		paletIconen: iconenLijst,
		afterLoad: ownProps.afterLoad,
		isEditable: ownProps.isEditable,
		amputatie: getMandatory(state.onderzoekByAfspraakId, state.navigation.afspraakId).amputatie,
	}
}

const VisueleInspectieAfbeeldingContainer = connect(mapStateToProps)(VisueleInspectieAfbeeldingView)
export default VisueleInspectieAfbeeldingContainer