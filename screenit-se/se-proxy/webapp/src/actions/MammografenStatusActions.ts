import type {MammograafStatus} from "../datatypes/connectiestatus/MammograafStatus"
import {calcSort} from "../util/Util"

export type MammografenStatusActions = VulMammografenStatusAction;
export const VUL_MAMMOGRAFEN_STATUS = "VUL_MAMMOGRAFEN_STATUS"
export type VulMammografenStatusAction = {
	type: "VUL_MAMMOGRAFEN_STATUS";
	statusList: Array<MammograafStatus>;
};
export const createActionVulMammografenStatus = (statusList: Array<MammograafStatus>): VulMammografenStatusAction => ({
	type: VUL_MAMMOGRAFEN_STATUS,
	statusList: statusList.sort((m1, m2) => calcSort(m1.aeTitle, m2.aeTitle)),
})