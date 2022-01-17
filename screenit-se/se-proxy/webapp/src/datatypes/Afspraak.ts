import type {ClientDto} from "./Client"
import type {OnderzoekDto, Onderzoekstatus} from "./Onderzoek"
import type {MammografieDto} from "./Mammografie"
import type {Tijdslot} from "./Planning"
import type {SignaleringDto} from "./Signalering"
import type {GeenHuisartsOption} from "./Huisarts"
import type {OpschortenReden} from "./OpschortenReden"

export type Identificatiesoort = "PASPOORT" | "RIJBEWIJS" | "IDENTITEITSKAART" | "OVERIG";
export type Afspraakstatus =
	"VERWACHT"
	| "INGESCHREVEN"
	| "ONDERZOEK"
	| "SIGNALEREN"
	| "BEEINDIGD"
	| "KWALITEITSOPNAME";
export const alleIdentificatieSoorten: Array<Identificatiesoort> = ["PASPOORT", "RIJBEWIJS", "IDENTITEITSKAART", "OVERIG"]

export class Afspraak implements Tijdslot {
	id: number
	vanafDatum: string
	vanafTijd: string
	clientId: number
	onderzoekId?: number
	status: Afspraakstatus
	uitnodigingsNr: number
	identificatiesoort?: Identificatiesoort
	identificatienummer?: string
	aantalOproepen: number
	aantalOpgekomen: number
	bezwaarAangevraagd: boolean
	bezwaarDoorgevoerdOpCentraal: boolean
	huisartsId?: number
	geenHuisartsOptie?: GeenHuisartsOption
	doorgevoerd: boolean
	centralAvailable: boolean
	eerderOnderbrokenInZelfdeRonde: boolean
	eerdereOpschortenReden?: OpschortenReden
	eerdereOpschortenRedenTekst?: string
	geforceerd?: boolean

	constructor(id: number, vanafDatum: string, vanafTijd: string, clientId: number, status: Afspraakstatus, uitnodigingsNr: number, aantalOproepen: number, aantalOpgekomen: number, bezwaarAangevraagd: boolean, bezwaarDoorgevoerdOpCentraal: boolean, doorgevoerd: boolean, centralAvailable: boolean, eerderOnderbrokenInZelfdeRonde: boolean) {
		this.id = id
		this.vanafDatum = vanafDatum
		this.vanafTijd = vanafTijd
		this.clientId = clientId
		this.status = status
		this.uitnodigingsNr = uitnodigingsNr
		this.aantalOpgekomen = aantalOpgekomen
		this.aantalOproepen = aantalOproepen
		this.bezwaarAangevraagd = bezwaarAangevraagd
		this.bezwaarDoorgevoerdOpCentraal = bezwaarDoorgevoerdOpCentraal
		this.doorgevoerd = doorgevoerd
		this.centralAvailable = centralAvailable
		this.eerderOnderbrokenInZelfdeRonde = eerderOnderbrokenInZelfdeRonde
	}
}

export type AfspraakDto = {
	id: number;
	vanaf: string;
	client: ClientDto;
	status: Afspraakstatus;
	uitnodigingsNr: number;
	identificatiesoort: Identificatiesoort;
	identificatienummer: string;
	huisartsId: number;
	geenHuisartsOptie?: GeenHuisartsOption;
	aantalOproepen: number;
	aantalOpgekomen: number;
	bezwaarAangevraagd: boolean;
	huidigOnderzoek: OnderzoekDto;
	mammografie: MammografieDto;
	signaleren: SignaleringDto;
	doorgevoerd: boolean;
	centralAvailable: boolean;
	eerderOnderbrokenInZelfdeRonde: boolean;
	eerdereOpschortenReden?: OpschortenReden;
	eerdereOpschortenRedenTekst?: string;
	geforceerd?: boolean;
};

export const daglijstStatusnaam = (afspraak: Afspraak, onderzoekStatus: Onderzoekstatus | undefined): string => {
	switch (afspraak.status) {
		case "VERWACHT":
			return "Verwacht"
		case "INGESCHREVEN":
			return "Ingeschreven"
		case "ONDERZOEK":
			return "Onderzoek"
		case "SIGNALEREN":
			return "Signaleren"
		case "BEEINDIGD":
			if (onderzoekStatus) {
				switch (onderzoekStatus) {
					case "ONDERBROKEN":
						return "Onderbroken"
					case "ONVOLLEDIG":
						return "Onvolledig"
					case "AFGEROND":
						return "Afgerond"
					default:
						throw Error("Ongeldige status actief bij afgeronde afspraak.")
				}
			}
			return ""
		default:
			return ""
	}
}

export const identificatiesoortNaam = (soort: Identificatiesoort): string => {
	switch (soort) {
		case "PASPOORT":
			return "Paspoort"
		case "RIJBEWIJS":
			return "Rijbewijs"
		case "IDENTITEITSKAART":
			return "Identiteitskaart"
		case "OVERIG":
			return "Overig"
		default:
			return ""
	}
}

export const getIdentificatieMaxLength = (identificatieSoort: Identificatiesoort | undefined): number => {
	switch (identificatieSoort) {
		case "PASPOORT":
		case "IDENTITEITSKAART":
			return 9
		case "RIJBEWIJS":
			return 10
		case "OVERIG":
			return 255
		default:
			return 0
	}
}

export const afspraakHash = (afspraak: Afspraak): string => {
	return afspraak.id + afspraak.status
}