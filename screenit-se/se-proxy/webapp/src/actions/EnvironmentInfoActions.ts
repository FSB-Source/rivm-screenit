import type {EnvironmentInfo} from "../datatypes/EnvironmentInfo"

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

	return action
}