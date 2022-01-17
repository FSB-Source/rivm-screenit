import type {SeAction} from "../actions/SeAction"

export type TransactionType =
	"INSCHRIJFGEGEVENS_OPSLAAN"
	| "ONDERZOEK_STARTEN"
	| "VISUELE_INSPECTIE_OPSLAAN"
	| "SIGNALEREN_OPSLAAN"
	| "INSCHRIJVEN_PASSANT"
	| "BEEINDIGDE_AFSPRAAK_DOORVOEREN"
	| "UITSCHRIJVEN_CLIENT"
	| "START_KWALITEITSOPNAME_TRANSACTION"
	| "BEEINDIG_KWALITEITSOPNAME_TRANSACTION"
	| "LOG_GEBEURTENIS_SE";

export type Transaction = {
	type: TransactionType;
	clientId?: number;
	uitnodigingsNr?: number;
	instellingGebruikerId?: number;
	afspraakVanafDatum: string;
	medewerkercode?: string;
	sessionId?: string;
	actions: Array<SeAction>;
};

export type Action = {
	type: string;
	[key: string]: any;
};