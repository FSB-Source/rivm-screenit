import type {SuboptimaleInsteltechniek} from "../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek"
import type {RedenFotobespreking} from "../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking"

export type MBBSignaleringActions =
	SuboptimaleInsteltechniekAction
	| RedenFotobesprekingAction
	| ExtraMedewerkerAction
	| MbbOpmerkingAction
	| RadioloogOpmerkingAction
	| OperatieRechtsAction
	| OperatieLinksAction
	| AanvullendeInformatieOperatieAction;
export const MAAK_SUBOPTIMALE_INSTELTECHNIEK = "MAAK_SUBOPTIMALE_INSTELTECHNIEK"
export type SuboptimaleInsteltechniekAction = {
	type: "MAAK_SUBOPTIMALE_INSTELTECHNIEK";
	afspraakId: number;
	suboptimaleInsteltechniek: SuboptimaleInsteltechniek | undefined;
};
export const createActionMaakSuboptimaleInsteltechniek = (afspraakId: number, suboptimaleInsteltechniek: SuboptimaleInsteltechniek | undefined): SuboptimaleInsteltechniekAction => ({
	type: MAAK_SUBOPTIMALE_INSTELTECHNIEK,
	afspraakId: afspraakId,
	suboptimaleInsteltechniek: suboptimaleInsteltechniek,
})
export const MAAK_REDEN_FOTOBESPREKING = "MAAK_REDEN_FOTOBESPREKING"
export type RedenFotobesprekingAction = {
	type: "MAAK_REDEN_FOTOBESPREKING";
	afspraakId: number;
	redenFotobespreking: RedenFotobespreking | undefined;
};
export const createActionMaakRedenFotobespreking = (afspraakId: number, redenFotobespreking: RedenFotobespreking | undefined): RedenFotobesprekingAction => ({
	type: MAAK_REDEN_FOTOBESPREKING,
	afspraakId: afspraakId,
	redenFotobespreking: redenFotobespreking,
})
export const MAAK_EXTRA_MEDEWERKER = "MAAK_EXTRA_MEDEWERKER"
export type ExtraMedewerkerAction = {
	type: "MAAK_EXTRA_MEDEWERKER";
	afspraakId: number;
	extraMedewerkerId: number | undefined;
};
export const createActionMaakExtraMedewerker = (afspraakId: number, extraMedewerkerId: number | undefined): ExtraMedewerkerAction => ({
	type: MAAK_EXTRA_MEDEWERKER,
	afspraakId: afspraakId,
	extraMedewerkerId: extraMedewerkerId,
})
export const MAAK_MBB_OPMERKING = "MAAK_MBB_OPMERKING"
export type MbbOpmerkingAction = {
	type: "MAAK_MBB_OPMERKING";
	afspraakId: number;
	opmerkingMbber: string;
};
export const createActionMaakMbbOpmerking = (afspraakId: number, opmerking: string): MbbOpmerkingAction => ({
	type: MAAK_MBB_OPMERKING,
	afspraakId: afspraakId,
	opmerkingMbber: opmerking,
})
export const MAAK_RADIOLOOG_OPMERKING = "MAAK_RADIOLOOG_OPMERKING"
export type RadioloogOpmerkingAction = {
	type: "MAAK_RADIOLOOG_OPMERKING";
	afspraakId: number;
	opmerkingVoorRadioloog: string;
};
export const createActionMaakRadioloogOpmerking = (afspraakId: number, opmerking: string): RadioloogOpmerkingAction => ({
	type: MAAK_RADIOLOOG_OPMERKING,
	afspraakId: afspraakId,
	opmerkingVoorRadioloog: opmerking,
})
export const OPERATIE_RECHTS = "OPERATIE_RECHTS"
export type OperatieRechtsAction = {
	type: "OPERATIE_RECHTS";
	afspraakId: number;
	operatieRechts: boolean;
};
export const createActionOperatieRechts = (afspraakId: number, operatieRechts: boolean): OperatieRechtsAction => ({
	type: OPERATIE_RECHTS,
	afspraakId: afspraakId,
	operatieRechts: operatieRechts,
})
export const OPERATIE_LINKS = "OPERATIE_LINKS"
export type OperatieLinksAction = {
	type: "OPERATIE_LINKS";
	afspraakId: number;
	operatieLinks: boolean;
};
export const createActionOperatieLinks = (afspraakId: number, operatieLinks: boolean): OperatieLinksAction => ({
	type: OPERATIE_LINKS,
	afspraakId: afspraakId,
	operatieLinks: operatieLinks,
})
export const MAAK_AANVULLENDE_INFORMATIE_OPERATIE = "MAAK_AANVULLENDE_INFORMATIE_OPERATIE"
export type AanvullendeInformatieOperatieAction = {
	type: "MAAK_AANVULLENDE_INFORMATIE_OPERATIE";
	afspraakId: number;
	aanvullendeInformatieOperatie: string;
};
export const createActionMaakAanvullendeInformatieOperatie = (afspraakId: number, aanvullendeInformatieOperatie: string): AanvullendeInformatieOperatieAction => ({
	type: MAAK_AANVULLENDE_INFORMATIE_OPERATIE,
	afspraakId: afspraakId,
	aanvullendeInformatieOperatie: aanvullendeInformatieOperatie,
})