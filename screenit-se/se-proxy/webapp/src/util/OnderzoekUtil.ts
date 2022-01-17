import {Afspraak} from "../datatypes/Afspraak"
import type {Client} from "../datatypes/Client"
import {createActionMaakDubbeleTijd, createActionMaakDubbeleTijdReden} from "../actions/AanvullendeInformatieActions"
import type {SeAction} from "../actions/SeAction"

export const createDubbeleTijdActions = (afspraak: Afspraak, client: Client): Array<SeAction> => {
	const result: Array<SeAction> = []
	result.push(createActionMaakDubbeleTijd(afspraak.id, client.id, client.doelgroep === "DUBBELE_TIJD"))
	result.push(createActionMaakDubbeleTijdReden(afspraak.id, client.id, client.doelgroep === "DUBBELE_TIJD" ? client.dubbeleTijdReden : undefined))
	return result
}