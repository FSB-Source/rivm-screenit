import {baseUrl, createClientHeaders} from "../util/ApiUtil"
import {store} from "../Store"
import {MELDING_SESSIE_NIET_GELDIG, persistentErrorToast, showErrorToastWithoutAutoClose} from "../util/ToastUtil"
import {GEEN_IDENTIFICATIE, logoutClient} from "./AuthenticatieRestclient"
import {resetTimeout} from "../util/TimeoutUtil"
import {NavigationActions, RestoredNavigationAction} from "../actions/NavigationActions"

export const putNavigationState = (navigationAction: NavigationActions | RestoredNavigationAction): void => {
	resetTimeout()

	const environmentInfo = store.getState().environmentInfo
	if (!environmentInfo || !environmentInfo.nfcEnabled) {
		return
	}

	const session = store.getState().session

	if (session && session.yubikeyIdentificatie !== GEEN_IDENTIFICATIE) {
		const clientHeaders = createClientHeaders()
		const yubikeyIdentificatie = session.yubikeyIdentificatie
		fetch(`${baseUrl}navigationState/${yubikeyIdentificatie}`, {
			method: "PUT",
			headers: clientHeaders,
			body: JSON.stringify(navigationAction),
		}).then((response) => {
			if (response.ok) {
			} else if (response.status === 400) {
				console.log(`Uitloggen door ongeldige sessie: ${yubikeyIdentificatie}`)
				logoutClient()
				showErrorToastWithoutAutoClose(MELDING_SESSIE_NIET_GELDIG)
			} else if (response.status === 408) {
				console.log(`Uitloggen door inactiviteit: ${yubikeyIdentificatie}`)
				logoutClient()
				persistentErrorToast("Uitgelogd wegens inactiviteit.")
			}
		}).catch((error) => {
			console.log(`Er ontstond een probleem tijdens het uitvoeren van fetch bij navigeren: ${error.message}`)
		})
	}
}
