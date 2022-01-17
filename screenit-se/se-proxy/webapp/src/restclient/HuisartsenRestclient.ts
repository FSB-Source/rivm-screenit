import {fetchApi} from "../util/ApiUtil"
import {store} from "../Store"
import {createActionVulHuisartsenById} from "../actions/HuisartsActions"
import type {Huisarts, HuisartsDto} from "../datatypes/Huisarts"

export const huisartsFromDto = (huisartsDto: HuisartsDto): Huisarts => {
	return huisartsDto as Huisarts
}

export const leesHuisartsen = (): void => {
	fetchApi("GET", "huisarts/all", deHuisartsDtos => {
		store.dispatch(createActionVulHuisartsenById(deHuisartsDtos.map((huisartsDto: HuisartsDto) => huisartsFromDto(huisartsDto))))
	})
}