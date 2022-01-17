import {connect} from "react-redux"
import TelefoonView, {TelefoonViewDispatchProps, TelefoonViewStateProps} from "./TelefoonView"
import {getMandatory} from "../../util/MapUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {createActionSetTelefoon1, createActionSetTelefoon2} from "../../actions/ClientActions"
import {RootState} from "../../Store"
import {Dispatch} from "redux"

export type TelefoonContainerProps = {
	telefoonnummer1?: string;
	telefoonnummer2?: string;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: TelefoonContainerProps): TelefoonViewStateProps => {
	const {clientId} = state.navigation
	return {
		...ownProps,
		clientId: clientId,
		clientGegevensForm: getMandatory(state.formsByFormId, "clientgegevens"),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): TelefoonViewDispatchProps => ({
	setTelefoon1(clientId: number, telefoon: string): void {
		dispatchActions(dispatch, createActionSetTelefoon1(clientId, telefoon))
	},
	setTelefoon2(clientId: number, telefoon: string): void {
		dispatchActions(dispatch, createActionSetTelefoon2(clientId, telefoon))
	},
})

const TelefoonContainer = connect(mapStateToProps, mapDispatchToProps)(TelefoonView)
export default TelefoonContainer