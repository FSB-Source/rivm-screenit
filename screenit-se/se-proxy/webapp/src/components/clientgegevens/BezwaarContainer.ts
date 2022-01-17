import {connect} from "react-redux"
import type {BezwaarDispatchProps, BezwaarStateProps} from "./BezwaarView"
import BezwaarView from "./BezwaarView"
import {createActionBezwaarAanvragen} from "../../actions/AfspraakActions"
import {dispatchActions} from "../../util/DispatchUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState, parentProps: BezwaarStateProps): BezwaarStateProps => {
	return {
		...parentProps,
		bezwaarDoorgevoerdOpCentraal: parentProps.isIngeschreven || parentProps.bezwaarDoorgevoerdOpCentraal,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): BezwaarDispatchProps => ({
	onBezwaarAangevraagd(afspraakId: number, bezwaarAangevraagd: boolean, isIngeschreven: boolean): void {
		if (!isIngeschreven) {
			dispatchActions(dispatch, createActionBezwaarAanvragen(afspraakId, bezwaarAangevraagd))
		}
	},

})

const BezwaarContainer = connect(mapStateToProps, mapDispatchToProps)(BezwaarView)
export default BezwaarContainer