import {connect} from "react-redux"
import type {VorigeOnderzoekenConfirmBtnKind, VorigeOnderzoekenViewDispatchProps, VorigeOnderzoekenViewStateProps} from "./VorigeOnderzoekenView"
import VorigeOnderzoekenView from "./VorigeOnderzoekenView"
import {createActionOnderzoekStarten} from "../../actions/OnderzoekActions"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {dispatchActions} from "../../util/DispatchUtil"
import {disablePrimarySeKnop} from "../../util/Util"
import {showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import type {SeAction} from "../../actions/SeAction"
import {RootState} from "../../Store"
import {Dispatch} from "redux"
import {navigateToOnderzoek} from "../../util/NavigationUtil"

export const vorigeOnderzoekenConfirmBtnKind = (afspraak: Afspraak): VorigeOnderzoekenConfirmBtnKind => afspraak.status === "INGESCHREVEN" ? "Onderzoek starten" : "Volgende"

export type VorigeOnderzoekenContainerProps = {
	afspraak: Afspraak;
	client: Client;
	gebruikersnaam?: string;
	setHeeftOudeBeeldenOpgevraagd: () => void;
};

const mapStateToProps = (state: RootState, ownProps: VorigeOnderzoekenContainerProps): VorigeOnderzoekenViewStateProps => {
	return {
		...ownProps,
		magOnderzoeken: state.autorisatie.onderzoeken,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): VorigeOnderzoekenViewDispatchProps => {
	return {
		onConfirm(client: Client, afspraak: Afspraak): void {
			switch (vorigeOnderzoekenConfirmBtnKind(afspraak)) {
				case "Onderzoek starten":
					disablePrimarySeKnop()
					const actions: Array<SeAction> = []
					const amputatie = client.vorigeOnderzoeken && client.vorigeOnderzoeken[0] && client.vorigeOnderzoeken[0].onderzoek?.amputatie
					const onderzoekStartenAction = createActionOnderzoekStarten(afspraak.id, amputatie)
					actions.push(onderzoekStartenAction)
					dispatchActions(dispatch, ...actions)
					navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
					putTransactionToScreenItCentraalPromise(afspraak, "ONDERZOEK_STARTEN", ...actions).then(() => {
						showWijzigingenOpgeslagenToast()
					})
					break
				case "Volgende":
					disablePrimarySeKnop()
					navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
					break
				default:
					break
			}
		},

	}
}

const VorigeOnderzoekenContainer = connect(mapStateToProps, mapDispatchToProps)(VorigeOnderzoekenView)
export default VorigeOnderzoekenContainer
