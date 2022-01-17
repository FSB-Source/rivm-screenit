import {connect} from "react-redux"
import {createActionKiesDaglijstDatum} from "../../actions/DaglijstDatumActions"
import DatumkiezerView, {DatumkiezerViewDispatchProps, DatumkiezerViewStateProps} from "./DatumkiezerView"
import {leesAfspraken} from "../../restclient/DaglijstRestclient"
import {leesPlanning} from "../../restclient/PlanningRestClient"
import {getDagverslag} from "../../restclient/DagverslagRestClient"
import {createActionNavigateToDaglijst, createActionNavigateToDagverslag} from "../../actions/NavigationActions"
import {datumInVerleden, isAfter, vandaagPlusDagen} from "../../util/DateUtil"
import {showErrorToast} from "../../util/ToastUtil"
import {RootState} from "../../Store"
import {Dispatch} from "redux"

const mapStateToProps = (state: RootState): DatumkiezerViewStateProps => {
	return {
		daglijstDatum: state.daglijstDatum,
		online: state.online,
		dagenDaglijstOphalenLimiet: state.environmentInfo?.dagenDaglijstOphalenLimiet,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): DatumkiezerViewDispatchProps => ({
	onChooseDate(newDate: string, online: boolean, limiet?: number): void {
		if (datumInVerleden(newDate) || (limiet !== undefined && isAfter(newDate, vandaagPlusDagen(limiet)))) {
			if (online) {
				dispatch(createActionKiesDaglijstDatum(newDate))
				leesAfspraken(newDate, createActionNavigateToDagverslag())
				leesPlanning(newDate)
				getDagverslag(newDate)
			} else {
				showErrorToast("Dagverslag kan niet worden geopend, SE is offline.")
			}
		} else {
			dispatch(createActionKiesDaglijstDatum(newDate))
			leesAfspraken(newDate, createActionNavigateToDaglijst())
			leesPlanning(newDate)
		}
	},
})

const DatumkiezerContainer = connect(mapStateToProps, mapDispatchToProps)(DatumkiezerView)
export default DatumkiezerContainer
