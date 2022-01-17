import type {KwaliteitsopnameReden} from "../components/kwaliteitsopname/KwaliteitsopnameView"
export const START_KWALITEITSOPNAME = "START_KWALITEITSOPNAME"
export const BEEINDIG_KWALITEITSOPNAME = "BEEINDIG_KWALITEITSOPNAME"
export type KwaliteitsopnameOrmAction = {
	type: string;
	seCode: string;
	reden: KwaliteitsopnameReden;
	patientID: string;
	accessionNumber: string;
	onderzoekscode: string;
};
export const createActionKwaliteitsopnameOrm = (type: string, seCode: string, reden: KwaliteitsopnameReden, patientID: string, accessionNumber: string, onderzoekscode: string): KwaliteitsopnameOrmAction => ({
	type,
	seCode,
	reden,
	patientID,
	accessionNumber,
	onderzoekscode,
})