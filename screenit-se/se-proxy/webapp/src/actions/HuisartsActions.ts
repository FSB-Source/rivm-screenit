import type {GeenHuisartsOption, Huisarts} from "../datatypes/Huisarts"

export type HuisartsActions = VulHuisartsenByIdAction | KiesHuisartsAction | KiesGeenHuisartsOptieAction;
export const VUL_HUISARTSEN_BY_ID = "VUL_HUISARTSEN_BY_ID"
export type VulHuisartsenByIdAction = {
	type: "VUL_HUISARTSEN_BY_ID";
	huisartsen: Array<Huisarts>;
};
export const createActionVulHuisartsenById = (huisartsen: Array<Huisarts>): VulHuisartsenByIdAction => {
	return {
		type: VUL_HUISARTSEN_BY_ID,
		huisartsen: huisartsen,
	}
}
export const KIES_HUISARTS = "KIES_HUISARTS"
export type KiesHuisartsAction = {
	type: "KIES_HUISARTS";
	afspraakId: number;
	huisartsId: number;
};
export const createActionKiesHuisarts = (afspraakId: number, huisartsId: number): KiesHuisartsAction => {
	return {
		type: KIES_HUISARTS,
		afspraakId,
		huisartsId,
	}
}
export const KIES_GEEN_HUISARTS_OPTIE = "KIES_GEEN_HUISARTS_OPTIE"
export type KiesGeenHuisartsOptieAction = {
	type: "KIES_GEEN_HUISARTS_OPTIE";
	afspraakId: number;
	geenHuisartsOptie: GeenHuisartsOption;
};
export const createActionKiesGeenHuisartsOptie = (afspraakId: number, geenHuisartsOptie: GeenHuisartsOption): KiesGeenHuisartsOptieAction => {
	return {
		type: KIES_GEEN_HUISARTS_OPTIE,
		afspraakId,
		geenHuisartsOptie,
	}
}