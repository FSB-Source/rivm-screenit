import type {ClearCacheAction} from "./ClearCacheActions"
import type {ClearAfsprakenAction} from "./AfspraakActions"

export type UpdateActions = QueueDaglijstVerversenAction | ClearAfsprakenAction | ClearCacheAction;
export const QUEUE_DAGLIJST_VERVERSEN = "QUEUE_DAGLIJST_VERVERSEN"
export type QueueDaglijstVerversenAction = {
	type: "QUEUE_DAGLIJST_VERVERSEN";
};
export const createActionQueueDaglijstVerversen = (): QueueDaglijstVerversenAction => ({
	type: QUEUE_DAGLIJST_VERVERSEN,
})