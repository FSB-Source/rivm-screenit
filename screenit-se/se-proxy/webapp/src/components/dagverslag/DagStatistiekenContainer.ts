import {connect} from "react-redux"
import DagStatistiekenView, {DagStatistiekenViewProps} from "./DagStatistiekenView"
import {getDagAfspraken} from "../../util/AfsprakenUtil"
import {RootState} from "../../Store"
import {getIfExists} from "../../util/MapUtil"

const mapStateToProps = (state: RootState): DagStatistiekenViewProps => {
	const huidigeDagAfspraken = getDagAfspraken(state.daglijstDatum)
	return {
		afspraken: huidigeDagAfspraken,
		onderzoeken: state.onderzoekByAfspraakId,
		afsprakenLength: huidigeDagAfspraken.length,
		dagverslag: getIfExists(state.dagverslag, state.daglijstDatum),
	}
}

const DagStatistiekenContainer = connect(mapStateToProps)(DagStatistiekenView)
export default DagStatistiekenContainer