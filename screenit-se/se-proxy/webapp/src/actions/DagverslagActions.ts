import type {Dagverslag} from "../datatypes/Dagverslag"

export type DagverslagActions = SetDagverslagAction | SetNietAfgeslotenVanafAction;

export const SET_DAGVERSLAG = "SET_DAGVERSLAG"
export type SetDagverslagAction = {
	type: "SET_DAGVERSLAG";
	datum: string;
	dagverslag: Dagverslag;
};
export const createActionSetDagverslag = (datum: string, dagverslag: Dagverslag): SetDagverslagAction => {
	return {
		type: SET_DAGVERSLAG,
		datum: datum,
		dagverslag: dagverslag,
	}
}

export const SET_NIET_AFGESLOTEN_DATUM = "SET_NIET_AFGESLOTEN_DATUM"
export type SetNietAfgeslotenVanafAction = {
	type: "SET_NIET_AFGESLOTEN_DATUM";
	nietAfgeslotenVanaf?: string;
};
export const createActionSetNietAfgeslotenVanaf = (datum?: string): SetNietAfgeslotenVanafAction => {
	return {
		type: SET_NIET_AFGESLOTEN_DATUM,
		nietAfgeslotenVanaf: datum,
	}
}