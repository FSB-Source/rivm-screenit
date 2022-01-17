import {getMandatory} from "./MapUtil"
import {store} from "../Store"

export const calcSort = (value1: string, value2: string): number => value1 === value2 ? 0 : value1 > value2 ? 1 : -1
export const disablePrimarySeKnop = (): void => {
	const primarySeKnop = document.getElementById("Popover-btn") as HTMLButtonElement

	if (primarySeKnop) {
		primarySeKnop.disabled = true
	}
}

export const aeTitle = (): string => {
	const huidigeMammograafId = store.getState().huidigeMammograafId
	if (huidigeMammograafId) {
		return getMandatory(store.getState().mammografenById, huidigeMammograafId).aeTitle
	} else {
		return ""
	}
}

export const seCode = (): string => {
	if (aeTitle().length === 0 || aeTitle().split("-").length === 0) {
		return ""
	}
	return aeTitle().substr(aeTitle().split("-")[0].length + 1, 6)
}