import VorigOnderzoekView, {VorigOnderzoekViewDispatchProps, VorigOnderzoekViewStateProps} from "./VorigOnderzoekView"
import {connect} from "react-redux"
import {putTransactionToScreenItCentraalPromiseZonderAfspraak} from "../../../restclient/TransactionRestclient"
import {sendStudyMessageToIMS} from "../../../util/ImsApiUtil"
import {createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald} from "../../../actions/LogGebeurtenisActions"
import {showWijzigingenOpgeslagenToast} from "../../../util/ToastUtil"
import {RootState} from "../../../Store"

const mapStateToProps = (state: RootState, ownProps: VorigOnderzoekViewStateProps): VorigOnderzoekViewStateProps => {
	return ownProps
}

const mapDispatchToProps = (): VorigOnderzoekViewDispatchProps => ({
	vorigeOnderzoekOphalen(clientId: number, uitnodigingsNr: number, username: string, bsn: string): void {
		sendStudyMessageToIMS(uitnodigingsNr, bsn, username)
		putTransactionToScreenItCentraalPromiseZonderAfspraak(clientId, uitnodigingsNr, "LOG_GEBEURTENIS_SE", createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald()).then(() => {
			showWijzigingenOpgeslagenToast()
		})
	},
})

const VorigOnderzoekContainer = connect(mapStateToProps, mapDispatchToProps)(VorigOnderzoekView)
export default VorigOnderzoekContainer