import type {OnvolledigOnderzoekOption} from "./visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek"
import type {OnderbrokenOnderzoekOption} from "./visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek"
import type {ExtraFotosReden} from "./visueleinspectie/aanvullendeinformatie/ExtraFotosReden"
import type {SuboptimaleInsteltechniek} from "./visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek"
import type {RedenFotobespreking} from "./visueleinspectie/mbbsignalering/RedenFotobespreking"

export type Onderzoekstatus = "ACTIEF" | "ONDERBROKEN" | "ONVOLLEDIG" | "AFGEROND";
export type Amputatie = "LINKERBORST" | "RECHTERBORST";
export type OnderzoekDto = {
	id: number;
	eerderMammogramZorginstellingId?: number;
	eerderMammogramJaartal?: number;
	extraMedewerkerId?: number;
	suboptimaleInsteltechniek?: SuboptimaleInsteltechniek;
	redenFotobespreking?: RedenFotobespreking;
	opmerkingMbber?: string;
	opmerkingVoorRadioloog?: string;
	operatieRechts: boolean;
	operatieLinks: boolean;
	amputatie?: Amputatie;
	aanvullendeInformatieOperatie?: string;
	status: Onderzoekstatus;
	onvolledigOnderzoek?: OnvolledigOnderzoekOption;
	onderbrokenOnderzoek?: OnderbrokenOnderzoekOption;
	extraFotosRedenen?: Array<ExtraFotosReden>;
	adviesHuisarts?: string;
};
export type Onderzoek = {
	eerderMammogramZorginstellingId?: number;
	eerderMammogramJaartal?: number;
	extraMedewerkerId?: number;
	suboptimaleInsteltechniek?: SuboptimaleInsteltechniek;
	redenFotobespreking?: RedenFotobespreking;
	opmerkingMbber?: string;
	opmerkingVoorRadioloog?: string;
	operatieRechts: boolean;
	operatieLinks: boolean;
	amputatie?: Amputatie;
	aanvullendeInformatieOperatie?: string;
	status: Onderzoekstatus;
	onvolledigOnderzoek?: OnvolledigOnderzoekOption;
	onderbrokenOnderzoek?: OnderbrokenOnderzoekOption;
	extraFotosRedenen?: Array<ExtraFotosReden>;
	adviesHuisarts?: string;
};