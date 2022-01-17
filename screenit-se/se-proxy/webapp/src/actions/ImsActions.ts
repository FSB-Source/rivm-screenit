export type ImsActions = SetStudyForImsAction;
export type SetStudyForImsAction = {
	type: "SET_STUDY_FOR_IMS";
	activeStudyForIms: number | undefined;
};
export const SET_STUDY_FOR_IMS = "SET_STUDY_FOR_IMS"
export const createActionSetStudyForIms = (uitnodigingsNummer?: number): SetStudyForImsAction => {
	return {
		type: SET_STUDY_FOR_IMS,
		activeStudyForIms: uitnodigingsNummer,
	}
}