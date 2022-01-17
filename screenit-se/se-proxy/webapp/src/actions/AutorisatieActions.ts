import type {RechtMetGeldigheid} from "../datatypes/RechtMetGeldigheid"

export type AutorisatieActions = SetAutorisatieAction;
export const SET_AUTORISATIE = "SET_AUTORISATIE"
export type SetAutorisatieAction = {
	type: "SET_AUTORISATIE";
	rechten: RechtMetGeldigheid;
};
export const createActionSetAutorisatie = (recht: RechtMetGeldigheid): SetAutorisatieAction => {
	return {
		type: SET_AUTORISATIE,
		rechten: recht,
	}
}