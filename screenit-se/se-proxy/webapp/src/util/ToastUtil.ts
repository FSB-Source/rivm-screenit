import {Id, toast} from "react-toastify"

export const MELDING_SESSIE_NIET_GELDIG = "Uw sessie is niet geldig. Sluit alle ScreenIT vensters, leg Yubikey opnieuw op de lezer en log opnieuw in."
export const MELDING_TECHNISCHE_FOUT = "Er is een technische fout opgetreden."
export const MELDING_DUBBELE_INSTANTIE = "Er is al een ScreenIT tabblad open, sluit dit tabblad."
export const MELDING_YUBIKEY_GEDETECTEERD = "Yubikey gedetecteerd, log in om uw sessie te starten."
export const MELDING_NFC_ERROR = "Communicatiefout met Yubikey-lezer. Herstart werkstation."
export const MELDING_WIJZIGINGEN_VERWERKT = "Wijzigingen verwerkt."
export const DEFAULT_TOAST_TIMEOUT = 5000

let toastIdWijzigingenOpgeslagen: Id | undefined = undefined
let toastIdYubikeyHerkend: Id | undefined = undefined
let toastIdError: Id | undefined = undefined
let toastIdWarning: Id | undefined = undefined
let toastIdNfcError: Id | undefined = undefined
const MINUUT = 60000

const dismissToastIfActive = (toastId?: Id): void => {
	if (toastId && toast.isActive(toastId)) {
		toast.dismiss(toastId)
	}
}

export const dismissYubikeyHerkendToast = (): void => {
	dismissToastIfActive(toastIdYubikeyHerkend)
}

export const dismissNfcErrorToast = (): void => {
	dismissToastIfActive(toastIdNfcError)
}

export const dismissAllToasts = (): void => {
	dismissToastIfActive(toastIdWijzigingenOpgeslagen)
	dismissYubikeyHerkendToast()
	dismissNfcErrorToast()
	dismissToastIfActive(toastIdError)
	dismissToastIfActive(toastIdWarning)

}

export const showWijzigingenOpgeslagenToast = (): void => {
	if (!toastIdWijzigingenOpgeslagen || !toast.isActive(toastIdWijzigingenOpgeslagen)) {
		toastIdWijzigingenOpgeslagen = toast.success(MELDING_WIJZIGINGEN_VERWERKT, {
			className: "success-color",
		})
	}
}

export const showYubikeyHerkendToast = (): void => {
	if (!toastIdYubikeyHerkend || !toast.isActive(toastIdYubikeyHerkend)) {
		toastIdYubikeyHerkend = toast.info(MELDING_YUBIKEY_GEDETECTEERD, {
			className: "info-color",
			autoClose: false,
		})
	}
}

export const showNfcErrorToast = (): void => {
	dismissYubikeyHerkendToast()

	if (!toastIdNfcError || !toast.isActive(toastIdNfcError)) {
		toastIdNfcError = toast.error(MELDING_NFC_ERROR, {
			className: "danger-color",
			autoClose: false,
		})
	}
}

export const showWarningToast = (message: string, viewTime?: number): void => {
	if (!toastIdWarning || !toast.isActive(toastIdWarning)) {
		toastIdWarning = toast.warn(message, {
			autoClose: viewTime ? viewTime : MINUUT,
		})
	}
}

export const showErrorToast = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	if (!toastIdError || !toast.isActive(toastIdError)) {
		toastIdError = toast.error(message, {
			className: "danger-color",
		})
	}
}

export const showErrorToastWithoutAutoClose = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	if (!toastIdError || !toast.isActive(toastIdError)) {
		toastIdError = toast.error(message, {
			className: "danger-color",
			autoClose: false,
		})
	}
}

export const persistentErrorToast = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	toast.error(message, {
		className: "danger-color",
	})
}

export const persistentSuccessToast = (message: string): void => {
	toast.success(message, {
		className: "success-color",
	})
}