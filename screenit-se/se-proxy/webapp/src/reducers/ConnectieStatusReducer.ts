import type {ConnectieStatus, ConnectieStatusLevel} from "../datatypes/connectiestatus/ConnectieStatus"
import type {ConnectieStatusActions} from "../actions/ConnectieStatusActions"
import {PUT_IMS_CONNECTIE_STATUS, PUT_MAMMOGRAAF_CONNECTIE_STATUS} from "../actions/ConnectieStatusActions"
import {nuISO} from "../util/DateUtil"
import {Reducer} from "redux"

const defaultConnectieStatus: ConnectieStatus = {
	mammograafConnectieStatusByAeTitle: new Map(),
	imsConnectieStatus: "WARN",
	imsConnectieStatusTimestamp: undefined,
}

const ConnectieStatusReducer: Reducer<ConnectieStatus, ConnectieStatusActions> = (stateSlice = defaultConnectieStatus, action) => {
	switch (action.type) {
		case PUT_MAMMOGRAAF_CONNECTIE_STATUS:
			const clone: Map<string, ConnectieStatusLevel> = new Map(stateSlice.mammograafConnectieStatusByAeTitle)
			clone.set(action.aeTitle, action.statusLevel)
			return {
				mammograafConnectieStatusByAeTitle: clone,
				imsConnectieStatus: stateSlice.imsConnectieStatus,
				imsConnectieStatusTimestamp: stateSlice.imsConnectieStatusTimestamp,
			}
		case PUT_IMS_CONNECTIE_STATUS:
			return {
				mammograafConnectieStatusByAeTitle: stateSlice.mammograafConnectieStatusByAeTitle,
				imsConnectieStatus: action.statusLevel,
				imsConnectieStatusTimestamp: nuISO(),
			}
		default:
			return stateSlice
	}
}

export default ConnectieStatusReducer