import type {Zorginstelling} from "../datatypes/Zorginstelling"

export type ZorginstellingActions = VulZorginstellingenAction;
export const VUL_ZORGINSTELLINGEN = "VUL_ZORGINSTELLINGEN"
export type VulZorginstellingenAction = {
	type: "VUL_ZORGINSTELLINGEN";
	zorginstellingen: Array<Zorginstelling>;
};
export const createActionVulZorginstellingen = (zorginstellingen: Array<Zorginstelling>): VulZorginstellingenAction => ({
	type: VUL_ZORGINSTELLINGEN,
	zorginstellingen: zorginstellingen,
})