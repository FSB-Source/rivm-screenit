import type {Amputatie, Onderzoek, Onderzoekstatus} from "../datatypes/Onderzoek"
import type {AfspraakDto} from "../datatypes/Afspraak"

export type OnderzoekActions =
	VulOnderzoekByAfspraakIdAction
	| OnderzoekStartenAction
	| OnderzoekOpslaanAction
	| OnderzoekAfrondenAction;
export const VUL_ONDERZOEK_BY_AFSPRAAK_ID = "VUL_ONDERZOEK_BY_AFSPRAAK_ID"
export type VulOnderzoekByAfspraakIdAction = {
	type: "VUL_ONDERZOEK_BY_AFSPRAAK_ID";
	afspraken: Array<AfspraakDto>;
};
export const createActionVulOnderzoekByAfspraakId = (afspraken: Array<AfspraakDto>): VulOnderzoekByAfspraakIdAction => ({
	type: VUL_ONDERZOEK_BY_AFSPRAAK_ID,
	afspraken: afspraken,
})
export const ONDERZOEK_STARTEN = "ONDERZOEK_STARTEN"
export type OnderzoekStartenAction = {
	type: "ONDERZOEK_STARTEN";
	afspraakId: number;
	amputatie?: Amputatie;
};
export const createActionOnderzoekStarten = (afspraakId: number, amputatie: Amputatie | undefined): OnderzoekStartenAction => {
	return {
		type: ONDERZOEK_STARTEN,
		afspraakId: afspraakId,
		amputatie: amputatie,
	}
}
export const ONDERZOEK_OPSLAAN = "ONDERZOEK_OPSLAAN"
export type OnderzoekOpslaanAction = {
	type: "ONDERZOEK_OPSLAAN";
	afspraakId: number;
	onderzoek: Onderzoek;
};
export const createActionOnderzoekOpslaan = (afspraakId: number, onderzoek: Onderzoek): OnderzoekOpslaanAction => {
	return {
		type: ONDERZOEK_OPSLAAN,
		afspraakId: afspraakId,
		onderzoek: onderzoek,
	}
}
export const ONDERZOEK_AFRONDEN = "ONDERZOEK_AFRONDEN"
export type OnderzoekAfrondenAction = {
	type: "ONDERZOEK_AFRONDEN";
	afspraakId: number;
	status: Onderzoekstatus;
};
export const createActionOnderzoekAfronden = (afspraakId: number, status: Onderzoekstatus): OnderzoekAfrondenAction => {
	return {
		type: ONDERZOEK_AFRONDEN,
		afspraakId: afspraakId,
		status: status,
	}
}