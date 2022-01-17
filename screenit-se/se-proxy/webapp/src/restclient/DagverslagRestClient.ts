import {fetchApiPromise} from "../util/ApiUtil"
import {store} from "../Store"
import {createActionSetDagverslag, createActionSetNietAfgeslotenVanaf} from "../actions/DagverslagActions"
import type {Dagverslag} from "../datatypes/Dagverslag"
import {persistentErrorToast} from "../util/ToastUtil"
import {datumFormaat} from "../util/DateUtil"
import {vulAfspraken} from "./DaglijstRestclient"

export const getDagverslag = (daglijstDatum: string): Promise<any> => {
	return fetchApiPromise("GET", `dagverslag/${daglijstDatum}`).then(result => {
		return result.json().then((dagverslag: Dagverslag) => {
			const nietAfgeslotenVanaf = dagverslag.nietAfgeslotenVanaf
			store.dispatch(createActionSetDagverslag(daglijstDatum, dagverslag))
			store.dispatch(createActionSetNietAfgeslotenVanaf(nietAfgeslotenVanaf))

			if (nietAfgeslotenVanaf && nietAfgeslotenVanaf !== store.getState().daglijstDatum) {
				fetchApiPromise("GET", `daglijst/geforceerd/${nietAfgeslotenVanaf}`).then(response => {
					response.json().then((daglijstMetMutaties: {
						daglijstJson: string;
						mutatieJsons: string[];
					}) => {
						if (daglijstMetMutaties) {
							vulAfspraken(nietAfgeslotenVanaf, undefined, JSON.parse(daglijstMetMutaties.daglijstJson), daglijstMetMutaties.mutatieJsons)
						}
					})
				})
				persistentErrorToast(`De werkdagen vanaf ${datumFormaat(nietAfgeslotenVanaf)} zijn niet afgesloten. Sluit deze af via het dagverslag.`)
			}

			return dagverslag
		})
	})
}