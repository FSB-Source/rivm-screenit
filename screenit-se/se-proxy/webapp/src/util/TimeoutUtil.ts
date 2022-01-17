import {store} from "../Store"
import {logoutClient} from "../restclient/AuthenticatieRestclient"
import {showErrorToast} from "./ToastUtil"
import {nu} from "./DateUtil"
import {Moment} from "moment"

let logoutMoment: Moment
let idleCheck: NodeJS.Timeout
export const ensureIdleCheck = (): void => {
	if (idleCheck) {
		clearInterval(idleCheck)
	}

	resetTimeout()
	idleCheck = setInterval(timerIncrement, 1000)
}
export const resetTimeout = (): void => {
	logoutMoment = nu().add(30, "minutes")
}

const timerIncrement = (): void => {
	const session = store.getState().session
	if (session && store.getState().online && nu() > logoutMoment) {
		const yubikeyIdentificatie = session.yubikeyIdentificatie
		console.log(`Uitloggen door 30 min inactiviteit: ${yubikeyIdentificatie}`)
		logoutClient()
		showErrorToast("Uitgelogd wegens inactiviteit")
	}
}