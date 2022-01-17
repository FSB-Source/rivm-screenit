import AkkoordPopupView, {AkkoordPopupViewDispatchProps, AkkoordPopupViewStateProps} from "./AkkoordPopupView"
import {dispatchActions} from "../../../util/DispatchUtil"
import {createActionAkkoordPopup, createActionClearPopup} from "../../../actions/PopupActions"
import {connect} from "react-redux"
import {Dispatch} from "redux"
import {RootState} from "../../../Store"

const mapStateToProps = (state: RootState): AkkoordPopupViewStateProps => {
	const popup = state.popup

	if (popup.visible) {
		return {
			visible: true,
			titel: popup.titel,
			body: popup.body,
			callback: popup.callback,
			cancelCallback: popup.cancelCallback,
			akkoordString: popup.akkoordString,
			annulerenString: popup.annulerenString,
			online: state.online,
			alleenOnline: popup.alleenOnline,
		}
	} else {
		return {
			visible: false,
		}
	}
}

const mapDispatchToProps = (dispatch: Dispatch): AkkoordPopupViewDispatchProps => ({
	akkoord(callback?: (...args: Array<any>) => any): void {
		if (callback) {
			callback()
		}
		dispatchActions(dispatch, createActionAkkoordPopup())
	},
	cancel(cancelCallback?: ((...args: Array<any>) => any)): void {
		if (cancelCallback) {
			cancelCallback()
		}
		dispatchActions(dispatch, createActionClearPopup())
	},
})

const AkkoordPopupContainer = connect(mapStateToProps, mapDispatchToProps)(AkkoordPopupView)
export default AkkoordPopupContainer