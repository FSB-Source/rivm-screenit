import {connect} from "react-redux"
import AfwijkingenView, {AfwijkingenViewProps} from "./AfwijkingenView"
import {signaleringPaletIconenLijst} from "./SignaleringPalet"
import {getIfExists} from "../../../util/MapUtil"
import type {Signalering} from "../../../datatypes/Signalering"
import type {Amputatie} from "../../../datatypes/Onderzoek"
import {RootState} from "../../../Store"

export type AfwijkingenContainerProps = {
	afspraakId: number;
	signalering: Signalering;
	isEditable: boolean;
	amputatie?: Amputatie;
};

const mapStateToProps = (state: RootState, ownProps: AfwijkingenContainerProps): AfwijkingenViewProps => {
	const signalering = getIfExists(state.signaleringByAfspraakId, state.navigation.afspraakId)
	const isEditable = ownProps.isEditable
	return {
		heeftAfwijkingen: ownProps.signalering.heeftAfwijkingen,
		magSignaleren: state.autorisatie.signaleren,
		afspraakId: ownProps.afspraakId,
		iconenByIdRechtsVerticaal: signalering && signalering.doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ? signalering.doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede.iconenById : new Map(),
		iconenByIdLinksVerticaal: signalering && signalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede ? signalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede.iconenById : new Map(),
		iconenByIdRechtsHorizontaal: signalering && signalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ? signalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede.iconenById : new Map(),
		iconenByIdLinksHorizontaal: signalering && signalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede ? signalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede.iconenById : new Map(),
		paletIconen: signaleringPaletIconenLijst,
		isEditable: isEditable,
		amputatie: ownProps.amputatie,
	}
}

const AfwijkingenContainer = connect(mapStateToProps)(AfwijkingenView)
export default AfwijkingenContainer