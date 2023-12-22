import {RootState} from "../Store"
import {Afspraak} from "../datatypes/Afspraak"
import {Onderzoekstatus} from "../datatypes/Onderzoek"
import {getIfExists} from "../util/MapUtil"

export const heeftAfspraakOnderzoekStatus = (state: RootState, afspraak: Afspraak, status: Onderzoekstatus): boolean => {
	const onderzoek = getIfExists(state.onderzoekByAfspraakId, afspraak.id)
	return onderzoek ? onderzoek.status === status : false
}