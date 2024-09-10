import {Reducer} from "redux"
import {UPDATE_WEBSOCKET_STATUS, WebsocketStatusActions} from "../actions/WebsocketStatusActions"
import {WEBSOCKET_STATUS_OFFLINE, WebsocketStatus} from "../datatypes/WebsocketStatus"

const WebsocketStatusReducer: Reducer<WebsocketStatus, WebsocketStatusActions> = (stateSlice = WEBSOCKET_STATUS_OFFLINE, action) => {
	switch (action.type) {
		case UPDATE_WEBSOCKET_STATUS:
			return action.websocketStatus
		default:
			return stateSlice
	}
}

export default WebsocketStatusReducer