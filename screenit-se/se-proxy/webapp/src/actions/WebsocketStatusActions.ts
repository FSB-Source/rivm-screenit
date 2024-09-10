import {WebsocketStatus} from "../datatypes/WebsocketStatus"

export type WebsocketStatusActions = UpdateWebsocketStatusAction;

export const UPDATE_WEBSOCKET_STATUS = "UPDATE_WEBSOCKET_STATUS"
export type UpdateWebsocketStatusAction = {
	type: "UPDATE_WEBSOCKET_STATUS"
	websocketStatus: WebsocketStatus
}

export const createUpdateWebsocketStatusAction = (websocketStatus: WebsocketStatus): UpdateWebsocketStatusAction => ({
	type: UPDATE_WEBSOCKET_STATUS,
	websocketStatus: websocketStatus,
})