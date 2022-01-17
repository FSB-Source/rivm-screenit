import {connect} from "react-redux"
import MammograafStatusView, {MammograafStatusViewDispatchProps, MammograafStatusViewStateProps} from "./MammograafStatusView"
import type {MammograafStatus} from "../../datatypes/connectiestatus/MammograafStatus"
import type {ConnectieStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"
import {Dispatch} from "redux"
import {createActionPutMammograafConnectieStatus} from "../../actions/ConnectieStatusActions"
import {getMandatory} from "../../util/MapUtil"
import {createActionShowPopup} from "../../actions/PopupActions"
import MammograafDicomFoutmeldingenPopupView from "./MammograafDicomFoutmeldingenPopupView"
import React from "react"
import type {MammograafDicomMessageType} from "../../datatypes/connectiestatus/MammograafDicomMessageError"
import {RootState} from "../../Store"

export type MammograafStatusContainerProps = {
	mammograafStatus: MammograafStatus;
};

const mapStateToProps = (state: RootState, ownProps: MammograafStatusContainerProps): MammograafStatusViewStateProps => {
	return {
		mammograafStatus: ownProps.mammograafStatus,
		mammograafConnectieStatusLevel: getMandatory(state.connectieStatus.mammograafConnectieStatusByAeTitle, ownProps.mammograafStatus.aeTitle),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): MammograafStatusViewDispatchProps => ({
	reportStatusLevel: (aeTitle: string, statusLevel: ConnectieStatusLevel): void => {
		dispatch(createActionPutMammograafConnectieStatus(aeTitle, statusLevel))
	},
	toonDicomFouten: (messageType: MammograafDicomMessageType, mammograafStatus: MammograafStatus): void => {
		dispatch(createActionShowPopup(`${messageType} fouten op ${mammograafStatus.aeTitle}`,
			<MammograafDicomFoutmeldingenPopupView
				messageType={messageType}
				errors={messageType === "DMWL" ? mammograafStatus.foutenSindsLaatsteSuccesDmwlBericht : mammograafStatus.foutenSindsLaatsteSuccesMppsBericht}/>))
	},
})

const MammograafStatusContainer = connect(mapStateToProps, mapDispatchToProps)(MammograafStatusView)
export default MammograafStatusContainer