import {store} from "../Store"
import {fetchApiPromise} from "../util/ApiUtil"
import {createActionSetHuidigeMammograaf, createActionVulMammografen} from "../actions/MammograafActions"
import {Mammograaf} from "../datatypes/Mammograaf"
import {persistentErrorToast} from "../util/ToastUtil"

export const readMammografen = (): Promise<Array<Mammograaf>> => {
	return new Promise((resolve, reject) => {
		fetchApiPromise("GET", "mammografen").then(response => {
			response.json().then((mammografen: Mammograaf[]) => {
				if (!mammografen.length) {
					persistentErrorToast("Voor deze SE is geen mammograaf geadministreerd, neem contact op met een beheerder.")
				}
				store.dispatch(createActionVulMammografen(mammografen))
				const environmentInfo = store.getState().environmentInfo
				if (environmentInfo && environmentInfo.environment !== "Test") {
					const huidigWerkstationIpAdres = environmentInfo.huidigWerkstationIpAdres
					const mammograaf = mammografen.find((m: Mammograaf) => m.werkstationIpAdres === huidigWerkstationIpAdres)
					if (mammograaf) {
						console.log(`Mammograaf gevonden voor ${huidigWerkstationIpAdres}: ${mammograaf.aeTitle}`)
						store.dispatch(createActionSetHuidigeMammograaf(mammograaf.id))
					} else {
						console.log(`Geen mammograaf gevonden voor ${environmentInfo.huidigWerkstationIpAdres} tussen ${JSON.stringify(mammografen)}`)
					}
				}

				resolve(mammografen)
			})
		}).catch(error => reject(error))
	})
}