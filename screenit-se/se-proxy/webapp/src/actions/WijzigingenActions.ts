export type WijzigingenActions =
	| WijzigingenVerwerktAction
	| WijzigingenGemaaktAction;

export const WIJZIGINGEN_VERWERKT = "WIJZIGINGEN_VERWERKT"
export type WijzigingenVerwerktAction = {
	type: "WIJZIGINGEN_VERWERKT";
};
export const createActionWijzigingenVerwerkt = (): WijzigingenVerwerktAction => ({
	type: WIJZIGINGEN_VERWERKT,
})

export const WIJZIGINGEN_GEMAAKT = "WIJZIGINGEN_GEMAAKT"
export type WijzigingenGemaaktAction = {
	type: "WIJZIGINGEN_GEMAAKT";
};
export const createActionWijzigingIdentificatie = (): WijzigingenGemaaktAction => ({
	type: WIJZIGINGEN_GEMAAKT,
})