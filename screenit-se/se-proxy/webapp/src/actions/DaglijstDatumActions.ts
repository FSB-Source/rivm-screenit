export type DaglijstDatumActions = KiesDaglijstDatumAction;

export const KIES_DAGLIJST_DATUM = "KIES_DAGLIJST_DATUM"
export type KiesDaglijstDatumAction = {
	type: "KIES_DAGLIJST_DATUM";
	datum: string;
};
export const createActionKiesDaglijstDatum = (datum: string): KiesDaglijstDatumAction => ({
	type: KIES_DAGLIJST_DATUM,
	datum: datum,
})