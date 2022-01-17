export type OpgehaaldeDagenActions = daglijstOpgehaaldAction;
export const DAGLIJST_OPGEHAALD = "DAGLIJST_OPGEHAALD"
export type daglijstOpgehaaldAction = {
	type: "DAGLIJST_OPGEHAALD";
	datum: string;
};
export const createActionDaglijstOpgehaald = (datum: string): daglijstOpgehaaldAction => ({
	type: DAGLIJST_OPGEHAALD,
	datum: datum,
})