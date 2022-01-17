import type {ConnectieStatusLevel} from "../datatypes/connectiestatus/ConnectieStatus"

export type ConnectieStatusActions = PutMammograafConnectieStatusAction | PutIMSConnectieStatusAction;
export const PUT_MAMMOGRAAF_CONNECTIE_STATUS = "PUT_MAMMOGRAAF_CONNECTIE_STATUS"
export type PutMammograafConnectieStatusAction = {
	type: "PUT_MAMMOGRAAF_CONNECTIE_STATUS";
	aeTitle: string;
	statusLevel: ConnectieStatusLevel;
};
export const createActionPutMammograafConnectieStatus = (aeTitle: string, statusLevel: ConnectieStatusLevel): PutMammograafConnectieStatusAction => ({
	type: PUT_MAMMOGRAAF_CONNECTIE_STATUS,
	aeTitle: aeTitle,
	statusLevel: statusLevel,
})
export const PUT_IMS_CONNECTIE_STATUS = "PUT_IMS_CONNECTIE_STATUS"
export type PutIMSConnectieStatusAction = {
	type: "PUT_IMS_CONNECTIE_STATUS";
	statusLevel: ConnectieStatusLevel;
};
export const createActionPutIMSConnectieStatus = (statusLevel: ConnectieStatusLevel): PutIMSConnectieStatusAction => ({
	type: PUT_IMS_CONNECTIE_STATUS,
	statusLevel: statusLevel,
})