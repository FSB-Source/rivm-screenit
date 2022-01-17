import {connect} from "react-redux"
import ConnectieStatusView, {ConnectieStatusViewProps} from "./ConnectieStatusView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): ConnectieStatusViewProps => {
	return {
		gebruikersNaam: state.session?.gebruikersnaam,
		seNaam: state.session?.seNaam,
		mammografen: Array.from(state.mammografenById.values()),
		mammografenStatus: state.mammografenStatus,
		connectieStatus: state.connectieStatus,
	}
}

const ConnectieStatusContainer = connect(mapStateToProps)(ConnectieStatusView)
export default ConnectieStatusContainer