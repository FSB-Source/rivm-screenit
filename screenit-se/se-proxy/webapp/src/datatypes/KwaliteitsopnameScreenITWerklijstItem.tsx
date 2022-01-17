import type {KwaliteitsopnameReden, VoorOfNaKalibratie} from "../components/kwaliteitsopname/KwaliteitsopnameView"

export type KwaliteitsopnameScreenITWerklijstItem = {
	aeTitle: string;
	medewerkercode: string;
	reden: KwaliteitsopnameReden;
	voorOfNaKalibratie: VoorOfNaKalibratie;
	seCode: string;
	accessionNumber: string;
	onderzoekscode: string;
	startMoment: string;
	patientId: string;
};