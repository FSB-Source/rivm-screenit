export type ConnectionActions = OfflineAction | OnlineAction;
export const SET_ONLINE = "SET_ONLINE"
export type OnlineAction = {
	type: "SET_ONLINE";
};
export const createActionOnline = (): OnlineAction => ({
	type: SET_ONLINE,
})
export const SET_OFFLINE = "SET_OFFLINE"
export type OfflineAction = {
	type: "SET_OFFLINE";
};
export const createActionOffline = (): OfflineAction => ({
	type: SET_OFFLINE,
})