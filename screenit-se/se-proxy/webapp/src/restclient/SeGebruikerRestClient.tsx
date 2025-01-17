import {fetchApi} from "../util/ApiUtil"
import {store} from "../Store"
import {createActionAddAllSeGebruikers} from "../actions/SeGebruikersActions"

export const readSeGebruikers = (): void => {
	fetchApi("GET", "seGebruiker", seGebruikers => {
		store.dispatch(createActionAddAllSeGebruikers(seGebruikers))
	})
}