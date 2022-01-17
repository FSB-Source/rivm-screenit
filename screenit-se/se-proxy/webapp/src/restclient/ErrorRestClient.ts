import {baseUrl, createClientHeaders} from "../util/ApiUtil"
import {store} from "../Store"

export type ConsoleMelding = {
	level: "LOG" | "ERROR" | "TRACE" | "WARN";
	melding: string;
	stack?: string;
}

export const verstuurConsoleMeldingNaarCentraal = (melding: ConsoleMelding): void => {
	const session = store.getState().session
	const medewerkerCode = session ? session.medewerkercode : "onbekend"
	fetch(`${baseUrl}console-melding/${medewerkerCode}`, {
		method: "POST",
		headers: createClientHeaders(),
		body: JSON.stringify({
			level: melding.level,
			melding: encodeURIComponent(melding.melding),
			stack: melding.stack,
		}),
	})
}
