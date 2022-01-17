export type DubbeleInstantieActions = DubbeleInsantieAction;
export const SET_DUBBELE_INSTANTIE = "SET_DUBBELE_INSTANTIE"
export type DubbeleInsantieAction = {
	type: "SET_DUBBELE_INSTANTIE";
};
export const createActionDubbeleInstantie = (): DubbeleInsantieAction => ({
	type: SET_DUBBELE_INSTANTIE,
})