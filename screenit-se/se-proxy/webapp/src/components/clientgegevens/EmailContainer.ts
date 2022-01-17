import {connect} from "react-redux"
import type {EmailViewDispatchProps, EmailViewStateProps} from "./EmailView"
import EmailView from "./EmailView"
import {createActionSetEmailAdres} from "../../actions/ClientActions"
import {getMandatory} from "../../util/MapUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"

export type EmailContainerProps = {
	clientId: number;
	emailadres?: string;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: EmailContainerProps): EmailViewStateProps => {
	return {
		...ownProps,
		clientGegevensForm: getMandatory(state.formsByFormId, "clientgegevens"),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): EmailViewDispatchProps => ({
	setEmailAdres(clientId: number, emailadres: string): void {
		dispatchActions(dispatch, createActionSetEmailAdres(clientId, emailadres))
	},
})

const EmailContainer = connect(mapStateToProps, mapDispatchToProps)(EmailView)
export default EmailContainer