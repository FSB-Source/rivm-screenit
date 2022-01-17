export type ClearCacheActions = ClearCacheAction;
export const CLEAR_CACHE = "CLEAR_CACHE"
export type ClearCacheAction = {
	type: "CLEAR_CACHE";
};
export const createActionClearCache = (): ClearCacheAction => ({
	type: CLEAR_CACHE,
})