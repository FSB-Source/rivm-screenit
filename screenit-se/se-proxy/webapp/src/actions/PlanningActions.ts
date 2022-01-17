import type {Planning} from "../datatypes/Planning"

export type PlanningActions = VulPlanningAction | ClearPlanningAction;
export const VUL_PLANNING = "VUL_PLANNING"
export type VulPlanningAction = {
	type: "VUL_PLANNING";
	datum: string;
	planning: Planning;
};
export const createActionVulPlanning = (datum: string, planning: Planning): VulPlanningAction => {
	return {
		type: VUL_PLANNING,
		datum: datum,
		planning: planning,
	}
}
export const CLEAR_PLANNING = "CLEAR_PLANNING"
export type ClearPlanningAction = {
	type: "CLEAR_PLANNING";
	datum: string;
};
export const createActionClearPlanning = (datum: string): ClearPlanningAction => {
	return {
		type: CLEAR_PLANNING,
		datum: datum,
	}
}