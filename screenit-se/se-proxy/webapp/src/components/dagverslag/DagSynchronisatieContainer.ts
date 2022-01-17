import {connect} from "react-redux"
import DagSynchronisatieView, {DagSynchronisatieViewStateProps} from "./DagSynchronisatieView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): DagSynchronisatieViewStateProps => {
	return {
		dagverslag: state.dagverslag,
		daglijstDatum: state.daglijstDatum,
	}
}

const DagSynchronisatieContainer = connect(mapStateToProps)(DagSynchronisatieView)
export default DagSynchronisatieContainer