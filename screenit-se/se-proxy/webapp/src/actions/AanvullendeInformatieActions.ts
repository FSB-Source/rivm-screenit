import type {OnvolledigOnderzoekOption} from "../datatypes/visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek"
import type {OnderbrokenOnderzoekOption} from "../datatypes/visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek"
import type {ExtraFotosReden} from "../datatypes/visueleinspectie/aanvullendeinformatie/ExtraFotosReden"
import type {Amputatie} from "../datatypes/Onderzoek"
import {OnderzoekType} from "../datatypes/OnderzoekType"

export type AanvullendeInformatieActions =
	EerderMammogramJaartalAction
	| EerderMammogramZorginstellingAction
	| OnvolledigOnderzoekAction
	| OnderbrokenOnderzoekAction
	| ExtraFotosRedenenAction
	| DubbeleTijdAction
	| DubbeleTijdRedenAction
	| AdviesHuisartsAction
	| SetAmputatieAction
	| SetOnderzoekTypeAction;
export const MAAK_EERDER_MAMMOGRAM_JAARTAL = "MAAK_EERDER_MAMMOGRAM_JAARTAL"
export type EerderMammogramJaartalAction = {
	type: "MAAK_EERDER_MAMMOGRAM_JAARTAL";
	afspraakId: number;
	eerderMammogramJaartal?: number;
};
export const createActionMaakEerderMammogramJaartal = (afspraakId: number, jaartal: number | undefined): EerderMammogramJaartalAction => ({
	type: MAAK_EERDER_MAMMOGRAM_JAARTAL,
	afspraakId: afspraakId,
	eerderMammogramJaartal: jaartal,
})
export const MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING = "MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING"
export type EerderMammogramZorginstellingAction = {
	type: "MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING";
	afspraakId: number;
	eerderMammogramZorginstellingId?: number;
};
export const createActionMaakEerderMammogramZorginstelling = (afspraakId: number, eerderMammogramZorginstellingId: number | undefined): EerderMammogramZorginstellingAction => ({
	type: MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING,
	afspraakId: afspraakId,
	eerderMammogramZorginstellingId: eerderMammogramZorginstellingId,
})
export const MAAK_ONVOLLEDIG_ONDERZOEK = "MAAK_ONVOLLEDIG_ONDERZOEK"
export type OnvolledigOnderzoekAction = {
	type: "MAAK_ONVOLLEDIG_ONDERZOEK";
	afspraakId: number;
	onvolledigOnderzoek: OnvolledigOnderzoekOption | undefined;
};
export const createActionMaakOnvolledigOnderzoek = (afspraakId: number, onvolledigOnderzoek: OnvolledigOnderzoekOption | undefined): OnvolledigOnderzoekAction => ({
	type: MAAK_ONVOLLEDIG_ONDERZOEK,
	afspraakId: afspraakId,
	onvolledigOnderzoek: onvolledigOnderzoek,
})
export const MAAK_ONDERBROKEN_ONDERZOEK = "MAAK_ONDERBROKEN_ONDERZOEK"
export type OnderbrokenOnderzoekAction = {
	type: "MAAK_ONDERBROKEN_ONDERZOEK";
	afspraakId: number;
	onderbrokenOnderzoek?: OnderbrokenOnderzoekOption;
};
export const createActionMaakOnderbrokenOnderzoek = (afspraakId: number, onderbrokenOnderzoek: OnderbrokenOnderzoekOption | undefined): OnderbrokenOnderzoekAction => ({
	type: MAAK_ONDERBROKEN_ONDERZOEK,
	afspraakId: afspraakId,
	onderbrokenOnderzoek: onderbrokenOnderzoek,
})
export const MAAK_EXTRA_FOTOS_REDENEN = "MAAK_EXTRA_FOTOS_REDENEN"
export type ExtraFotosRedenenAction = {
	type: "MAAK_EXTRA_FOTOS_REDENEN";
	afspraakId: number;
	extraFotosRedenen?: Array<ExtraFotosReden>;
};
export const createActionMaakExtraFotosRedenen = (afspraakId: number, extraFotosRedenen: Array<ExtraFotosReden> | undefined): ExtraFotosRedenenAction => ({
	type: MAAK_EXTRA_FOTOS_REDENEN,
	afspraakId: afspraakId,
	extraFotosRedenen: extraFotosRedenen,
})
export const MAAK_ONDERZOEK_TYPE = "MAAK_ONDERZOEK_TYPE"
export type SetOnderzoekTypeAction = {
	type: "MAAK_ONDERZOEK_TYPE";
	afspraakId: number;
	onderzoekType: OnderzoekType;
}
export const createActionSetOnderzoekType = (afspraakId: number, onderzoekType: OnderzoekType): SetOnderzoekTypeAction => ({
	type: MAAK_ONDERZOEK_TYPE,
	afspraakId: afspraakId,
	onderzoekType: onderzoekType,
})
export const MAAK_DUBBELE_TIJD = "MAAK_DUBBELE_TIJD"
export type DubbeleTijdAction = {
	type: "MAAK_DUBBELE_TIJD";
	afspraakId: number;
	clientId: number;
	dubbeleTijd: boolean;
};
export const createActionMaakDubbeleTijd = (afspraakId: number, clientId: number, dubbeleTijd: boolean): DubbeleTijdAction => ({
	type: MAAK_DUBBELE_TIJD,
	afspraakId: afspraakId,
	clientId: clientId,
	dubbeleTijd: dubbeleTijd,
})
export const MAAK_DUBBELE_TIJD_REDEN = "MAAK_DUBBELE_TIJD_REDEN"
export type DubbeleTijdRedenAction = {
	type: "MAAK_DUBBELE_TIJD_REDEN";
	afspraakId: number;
	clientId: number;
	dubbeleTijdReden?: string;
};
export const createActionMaakDubbeleTijdReden = (afspraakId: number, clientId: number, dubbeleTijdReden: string | undefined): DubbeleTijdRedenAction => ({
	type: MAAK_DUBBELE_TIJD_REDEN,
	afspraakId: afspraakId,
	clientId: clientId,
	dubbeleTijdReden: dubbeleTijdReden,
})
export const MAAK_ADVIES_HUISARTS = "MAAK_ADVIES_HUISARTS"
export type AdviesHuisartsAction = {
	type: "MAAK_ADVIES_HUISARTS";
	afspraakId: number;
	adviesHuisarts: string;
};
export const createActionMaakAdviesHuisarts = (afspraakId: number, adviesHuisarts: string): AdviesHuisartsAction => ({
	type: MAAK_ADVIES_HUISARTS,
	afspraakId: afspraakId,
	adviesHuisarts: adviesHuisarts,
})
export const SET_AMPUTATIE = "SET_AMPUTATIE"
export type SetAmputatieAction = {
	type: "SET_AMPUTATIE";
	afspraakId: number;
	amputatie?: Amputatie;
};
export const createActionSetAmputatie = (afspraakId: number, amputatie: Amputatie | undefined): SetAmputatieAction => ({
	type: SET_AMPUTATIE,
	afspraakId: afspraakId,
	amputatie: amputatie,
})