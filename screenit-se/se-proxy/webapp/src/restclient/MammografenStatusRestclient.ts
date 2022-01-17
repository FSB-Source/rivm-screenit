import {store} from "../Store"
import {fetchApiPromise} from "../util/ApiUtil"
import {createActionVulMammografenStatus} from "../actions/MammografenStatusActions"
import type {MammograafStatus} from "../datatypes/connectiestatus/MammograafStatus"
import {createActionPutMammograafConnectieStatus} from "../actions/ConnectieStatusActions"
import {Mammograaf} from "../datatypes/Mammograaf"

export const readMammografenStatus = (mammografen: Array<Mammograaf>): Promise<void> => {
	return new Promise(resolve => {
		fetchApiPromise("GET", `mammografenstatus?aeTitles=${encodeURIComponent(mammografen.map(m => m.aeTitle).toString())}`).then(response => {
			response.json().then((statusList: Array<MammograafStatus>) => {
				store.dispatch(createActionVulMammografenStatus(statusList))
				statusList.forEach(m => store.dispatch(createActionPutMammograafConnectieStatus(m.aeTitle, "WARN")))
				resolve()
			})
		})
	})
}