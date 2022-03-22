import {connect} from "react-redux"
import React from "react"
import DagverslagView, {DagverslagViewDispatchProps, DagverslagViewStateProps} from "./DagverslagView"
import {createActionShowPopup} from "../../actions/PopupActions"
import AfsprakenDoorvoerenView from "./AfsprakenDoorvoerenView"
import {Afspraak} from "../../datatypes/Afspraak"
import {RootState, store} from "../../Store"
import {createTransaction, putTransactionsToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {createActionAfspraakDoorvoeren} from "../../actions/AfspraakActions"
import {hasNietDoorgevoerdeOnderzoeken, hasOpenstaandeOnderzoeken} from "../../util/AfsprakenUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {showErrorToast, showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import {createActionKiesDaglijstDatum} from "../../actions/DaglijstDatumActions"
import {vandaagISO} from "../../util/DateUtil"
import type {SeAction} from "../../actions/SeAction"
import type {Transaction} from "../../datatypes/Transaction"
import {Dispatch} from "redux"
import {navigateToDaglijst} from "../../util/NavigationUtil"

const mapStateToProps = (state: RootState): DagverslagViewStateProps => {
	const voorgaandeDagAfgesloten = !state.nietAfgeslotenVanaf || state.nietAfgeslotenVanaf === state.daglijstDatum
	const openstaandeOnderzoeken = hasOpenstaandeOnderzoeken(state.daglijstDatum)
	const nietDoorgevoerdeOnderzoeken = hasNietDoorgevoerdeOnderzoeken(state.daglijstDatum)
	const afsprakenDoorvoerenEnabled = !openstaandeOnderzoeken && nietDoorgevoerdeOnderzoeken && voorgaandeDagAfgesloten
	const doorvoerenFeedback = maakFeedback(openstaandeOnderzoeken, nietDoorgevoerdeOnderzoeken, voorgaandeDagAfgesloten)
	return {
		afsprakenDoorvoerenDisabled: !afsprakenDoorvoerenEnabled,
		doorvoerenFeedback: doorvoerenFeedback,
		magOnderzoeken: state.autorisatie.onderzoeken,
		online: state.online,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): DagverslagViewDispatchProps => ({
	showAfsprakenDoorvoerenPopup(afsprakenDoorvoerenDisabled: boolean, doorvoerenFeedback: string): void {
		if (afsprakenDoorvoerenDisabled) {
			showErrorToast(doorvoerenFeedback)
		} else {
			dispatch(createActionShowPopup("Alle afgeronde afspraken doorvoeren",
				<AfsprakenDoorvoerenView/>, () => {
					afsprakenDoorvoeren()
				}, undefined, "Akkoord", "Annuleren"))
		}
	},
})

const afsprakenDoorvoeren = (): void => {
	const huidigeDagAfspraken: Array<Afspraak> = [...store.getState().afsprakenById.values()].filter((afspraak: Afspraak) => afspraak.vanafDatum === store.getState().daglijstDatum)
	const afsprakenDoorvoerenTransactions: Array<Transaction> = []
	const afsprakenDoorvoerenActions: Array<SeAction> = []
	huidigeDagAfspraken.forEach((afspraak: Afspraak) => {
		if (afspraak.status === "BEEINDIGD" && !afspraak.doorgevoerd) {
			const afspraakDoorvoerenAction = createActionAfspraakDoorvoeren(afspraak.id)
			afsprakenDoorvoerenActions.push(afspraakDoorvoerenAction)
			afsprakenDoorvoerenTransactions.push(createTransaction(afspraak, "BEEINDIGDE_AFSPRAAK_DOORVOEREN", afspraakDoorvoerenAction))
		}
	})

	if (afsprakenDoorvoerenActions.length > 0) {
		dispatchActions(store.dispatch, ...afsprakenDoorvoerenActions)
		putTransactionsToScreenItCentraalPromise(afsprakenDoorvoerenTransactions).then(() => {
			showWijzigingenOpgeslagenToast()
			store.dispatch(createActionKiesDaglijstDatum(vandaagISO()))
			navigateToDaglijst(store.dispatch)
		})
	}
}

const maakFeedback = (openstaandeOnderzoeken: boolean, nietDoorgevoerdeOnderzoeken: boolean, voorgaandeDagAfgesloten: boolean): string => {
	const redenen: string[] = []
	if (!voorgaandeDagAfgesloten) {
		redenen.push("De vorige werkdag is niet correct afgesloten.")
	}
	if (openstaandeOnderzoeken) {
		redenen.push("Er zijn onderzoeken in bewerking.")
	}
	if (!nietDoorgevoerdeOnderzoeken) {
		redenen.push("Geen afgesloten onderzoeken om door te voeren.")
	}
	return redenen.join(" ")
}

const DagverslagContainer = connect(mapStateToProps, mapDispatchToProps)(DagverslagView)
export default DagverslagContainer
