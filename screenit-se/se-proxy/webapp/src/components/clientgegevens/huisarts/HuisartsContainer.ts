import {connect} from "react-redux"
import type {HuisartsViewDispatchProps, HuisartsViewStateProps} from "./HuisartsView"
import HuisartsView from "../huisarts/HuisartsView"
import {getIfExists, getMandatory} from "../../../util/MapUtil"
import {createActionKiesGeenHuisartsOptie, createActionKiesHuisarts} from "../../../actions/HuisartsActions"
import type {GeenHuisartsOption} from "../../../datatypes/Huisarts"
import {Huisarts} from "../../../datatypes/Huisarts"
import type {Afspraak} from "../../../datatypes/Afspraak"
import type {Client} from "../../../datatypes/Client"
import {dispatchActions} from "../../../util/DispatchUtil"
import {RootState} from "../../../Store"
import {Dispatch} from "redux"

export type HuisartsContainerProps = {
	afspraak: Afspraak;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: HuisartsContainerProps): HuisartsViewStateProps | undefined => {
	if (ownProps.afspraak) {
		const huisartsId = getMandatory(state.afsprakenById, ownProps.afspraak.id).huisartsId
		const huisarts = huisartsId ? getIfExists(state.huisartsenById, huisartsId) : undefined
		const client: Client = getMandatory(state.clientenById, ownProps.afspraak.clientId)
		const geenHuisartsOptie = ownProps.afspraak.geenHuisartsOptie
		const form = getMandatory(state.formsByFormId, "clientgegevens")
		return {
			afspraak: ownProps.afspraak,
			disabled: ownProps.disabled,
			clientPlaats: client.adres.plaats,
			huisarts: huisarts,
			geenHuisartsOptie: geenHuisartsOptie,
			clientGegevensForm: form,
			isValid: (form.isSubmitted && !!(huisarts || geenHuisartsOptie)) || !form.isSubmitted,
		}
	}
}

const mapDispatchToProps = (dispatch: Dispatch): HuisartsViewDispatchProps => ({
	onKiesHuisarts(afspraak: Afspraak, huisarts: Huisarts): void {
		dispatchActions(dispatch, createActionKiesHuisarts(afspraak.id, huisarts.id))
	},
	onKiesGeenHuisartsOptie(afspraak: Afspraak, geenHuisartsOptie: GeenHuisartsOption): void {
		dispatchActions(dispatch, createActionKiesGeenHuisartsOptie(afspraak.id, geenHuisartsOptie))
	},
})

const HuisartsContainer = connect(mapStateToProps, mapDispatchToProps)(HuisartsView)
export default HuisartsContainer