import type {EnvironmentInfo} from "../datatypes/EnvironmentInfo"
import {AANTAL_INTERVALLEN_VOOR_LOGOUT, startPollingNfc} from "../util/NfcUtil"

export type EnvironmentInfoActions = SetEnvironmentInfoAction;

export const SET_ENVIRONMENTINFO = "SET_ENVIRONMENTINFO"
export type SetEnvironmentInfoAction = {
	type: "SET_ENVIRONMENTINFO";
	environment: EnvironmentInfo;
};
export const createActionSetEnvironmentInfo = (environmentInfo: EnvironmentInfo): SetEnvironmentInfoAction => {
	const action: SetEnvironmentInfoAction = {
		type: SET_ENVIRONMENTINFO,
		environment: environmentInfo,
	}

	if (environmentInfo.nfcEnabled) {
		startPollingNfc(AANTAL_INTERVALLEN_VOOR_LOGOUT)
	}

	return action
}