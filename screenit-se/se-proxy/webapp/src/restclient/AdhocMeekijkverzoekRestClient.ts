import {baseUrl, createClientHeaders} from "../util/ApiUtil"
import {persistentSuccessToast} from "../util/ToastUtil"

export const verstuurAdHocVerzoek = (reden: string, afspraakId: number): void => {
	fetch(`${baseUrl}adhocMeekijkverzoek/indienen/${afspraakId}`, {
		method: "POST",
		headers: createClientHeaders(),
		body: reden,
	})
	persistentSuccessToast(`Meekijkverzoek LRCB verzonden met reden: ${reden}`)
}
export const magAdhocVersturen = (afspraakId: number): Promise<number> => {
	return fetch(`${baseUrl}adhocMeekijkverzoek/controleren/${afspraakId}`, {
		method: "POST",
		headers: createClientHeaders(),
	}).then(response => {
		return response.status
	}).catch(() => {
		return 500
	})
}