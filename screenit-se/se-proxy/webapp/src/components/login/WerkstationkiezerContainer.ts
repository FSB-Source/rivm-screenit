import {connect} from "react-redux"
import WerkstationkiezerView, {WerkstationkiezerProps} from "./WerkstationkiezerView"
import {calcSort} from "../../util/Util"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): WerkstationkiezerProps => {
	return {
		mammografen: Array.from(state.mammografenById.values()).sort((m1, m2) => calcSort(m1.aeTitle, m2.aeTitle)),
	}
}

const WerkstationkiezerContainer = connect(mapStateToProps)(WerkstationkiezerView)
export default WerkstationkiezerContainer