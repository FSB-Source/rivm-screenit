import {connect} from "react-redux"
import DagproductieView, {DagproductieViewProps} from "./DagproductieView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): DagproductieViewProps => {
	return {
		daglijstDatum: state.daglijstDatum,
		dagverslag: state.dagverslag,
	}
}

const DagproductieContainer = connect(mapStateToProps)(DagproductieView)
export default DagproductieContainer