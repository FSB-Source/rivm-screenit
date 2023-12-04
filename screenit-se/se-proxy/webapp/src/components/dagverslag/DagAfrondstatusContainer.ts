import {connect} from "react-redux"
import DagAfrondstatusView, {DagAfrondstatusViewStateProps} from "./DagAfrondstatusView"
import {RootState} from "../../Store"
import {getTotaalAfsprakenAfgerondDag} from "../../selectors/AfspraakSelectors"

const mapStateToProps = (state: RootState): DagAfrondstatusViewStateProps => {
	return {
		dagverslag: state.dagverslag,
		daglijstDatum: state.daglijstDatum,
		totaalAfgerond: getTotaalAfsprakenAfgerondDag(state),
	}
}

const DagAfrondstatusContainer = connect(mapStateToProps)(DagAfrondstatusView)
export default DagAfrondstatusContainer