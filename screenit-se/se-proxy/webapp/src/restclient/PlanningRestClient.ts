import {store} from "../Store"
import {fetchApiPromise} from "../util/ApiUtil"
import {createActionVulPlanning} from "../actions/PlanningActions"
import type {PlanningDto} from "../datatypes/Planning"
import {GeenScreeningBlok, GeenScreeningBlokDto} from "../datatypes/Planning"
import {getDate, getTime} from "../util/DateUtil"

const geenScreeningBlokFromDto = (geenScreeningblokDto: GeenScreeningBlokDto): GeenScreeningBlok => {
	return new GeenScreeningBlok(
		getDate(geenScreeningblokDto.vanaf),
		getTime(geenScreeningblokDto.vanaf),
		geenScreeningblokDto.tot,
		geenScreeningblokDto.opmerking,
	)
}

export const leesPlanning = (datum: string): void => {
	fetchApiPromise("GET", `planning/${datum}`).then(response => {
		if (response.ok) {
			response.json().then((planningDto: PlanningDto) => {
				store.dispatch(createActionVulPlanning(datum, {
					geenScreeningBlokken: planningDto.geenScreeningBlokken.map(geenScreeningBlokFromDto),
				}))
			})
		} else {
			console.warn(`LeesPlanning: geen ok response maar: ${response.status}`)
		}
	})
}