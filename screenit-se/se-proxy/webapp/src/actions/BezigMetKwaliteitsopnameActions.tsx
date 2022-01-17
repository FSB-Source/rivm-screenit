export const START_BEZIG_MET_KWALITEITSOPNAME = "START_BEZIG_MET_KWALITEITSOPNAME"
export const BEEINDIG_BEZIG_MET_KWALITEITSOPNAME = "BEEINDIG_BEZIG_MET_KWALITEITSOPNAME"
export type BezigMetKwaliteitsopnameAction = {
	type: string;
	volgnr?: number;
};
export const createStartBezigMetKwaliteitsopnameAction = (volgnr: number): BezigMetKwaliteitsopnameAction => ({
	type: START_BEZIG_MET_KWALITEITSOPNAME,
	volgnr,
})
export const createBeeindigBezigMetKwaliteitsopnameAction = (volgnr: number): BezigMetKwaliteitsopnameAction => ({
	type: BEEINDIG_BEZIG_MET_KWALITEITSOPNAME,
	volgnr,
})