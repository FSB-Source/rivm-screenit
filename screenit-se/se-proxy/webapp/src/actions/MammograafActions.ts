import {Mammograaf} from "../datatypes/Mammograaf"
export type MammograafActions = VulMammografenAction | SetHuidigeMammograafAction;
export const VUL_MAMMOGRAFEN = "VUL_MAMMOGRAFEN"
export type VulMammografenAction = {
	type: "VUL_MAMMOGRAFEN";
	mammografen: Array<Mammograaf>;
};
export const createActionVulMammografen = (mammografen: Array<Mammograaf>): VulMammografenAction => {
	return {
		type: VUL_MAMMOGRAFEN,
		mammografen,
	}
}

export const SET_HUIDIGE_MAMMOGRAAF = "SET_HUIDIGE_MAMMOGRAAF"
export type SetHuidigeMammograafAction = {
	type: "SET_HUIDIGE_MAMMOGRAAF";
	mammograafId: number;
};
export const createActionSetHuidigeMammograaf = (mammograafId: number): SetHuidigeMammograafAction => {
	return {
		type: SET_HUIDIGE_MAMMOGRAAF,
		mammograafId,
	}
}