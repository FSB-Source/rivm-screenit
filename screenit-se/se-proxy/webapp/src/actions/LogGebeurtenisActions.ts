export type LogGebeurtenisActions =
	LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction
	| LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction
	| LogGebeurtenisBeeldenGeenMppsOntvangenAction;
export const LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD = "LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD"
export type LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction = {
	type: "LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD";
	logMessage: string;
};
export const createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald = (): LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction => {
	return {
		type: LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD,
		logMessage: "Er zijn beelden opgehaald uit vorige ronde.",
	}
}
export const LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH = "LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH"
export type LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction = {
	type: "LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH";
	logMessage: string;
};
export const createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch = (): LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction => {
	return {
		type: LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH,
		logMessage: "Verschil tussen zijde beelden en zijde amputatiekruis, gebruiker heeft geaccordeerd.",
	}
}
export const LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN = "LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN"
export type LogGebeurtenisBeeldenGeenMppsOntvangenAction = {
	type: "LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN";
	logMessage: string;
};
export const createActionLogGebeurtenisBeeldenGeenMppsOntvangen = (): LogGebeurtenisBeeldenGeenMppsOntvangenAction => {
	return {
		type: LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN,
		logMessage: "Er is een waarschuwing getoond voor het ontbreken van een MPPS bericht.",
	}
}