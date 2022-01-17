import {connect} from "react-redux"
import DagAfrondstatusView, {DagAfrondstatusViewStateProps} from "./DagAfrondstatusView"
import {Afspraak} from "../../datatypes/Afspraak"
import {getDagAfspraken} from "../../util/AfsprakenUtil"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): DagAfrondstatusViewStateProps => {
	const huidigeDagAfspraken: Array<Afspraak> = getDagAfspraken(state.daglijstDatum)
	return {
		dagverslag: state.dagverslag,
		daglijstDatum: state.daglijstDatum,
		totaalAfgerond: getTotaalAfgerond(huidigeDagAfspraken),
	}
}

const getTotaalAfgerond = (afspraken: Array<Afspraak>): number => {
	let totalResult = 0
	afspraken.forEach((afspraak) => {
		if (afspraak.status === "BEEINDIGD") {
			totalResult += 1
		}
	})
	return totalResult
}

const DagAfrondstatusContainer = connect(mapStateToProps)(DagAfrondstatusView)
export default DagAfrondstatusContainer