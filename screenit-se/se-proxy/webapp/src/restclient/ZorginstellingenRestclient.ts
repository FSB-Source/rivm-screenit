import {fetchApi} from "../util/ApiUtil"
import type {Zorginstelling, ZorginstellingDto} from "../datatypes/Zorginstelling"
import {createActionVulZorginstellingen} from "../actions/ZorginstellingActions"
import {store} from "../Store"

export const zorginstellingFromDto = (zorginstellingDto: ZorginstellingDto): Zorginstelling => {
	return zorginstellingDto as Zorginstelling
}

export const leesZorginstellingen = (): void => {
	fetchApi("GET", "zorginstelling/metMammapoliOfRadiologie", zorginstellingenDtos => {
		store.dispatch(createActionVulZorginstellingen(zorginstellingenDtos.map((zorginstellingDto: ZorginstellingDto) => zorginstellingFromDto(zorginstellingDto))))
	})
}