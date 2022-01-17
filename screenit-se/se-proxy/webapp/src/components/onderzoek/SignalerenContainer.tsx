import {connect} from "react-redux"
import type {SignalerenViewDispatchProps, SignalerenViewStateProps} from "./SignalerenView"
import SignalerenView from "./SignalerenView"
import {createActionAfspraakAfronden, createActionAfspraakSignalerenOpslaan} from "../../actions/AfspraakActions"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import {createActionOnderzoekAfronden, createActionOnderzoekOpslaan} from "../../actions/OnderzoekActions"
import type {Onderzoek} from "../../datatypes/Onderzoek"
import type {Signalering} from "../../datatypes/Signalering"
import {mapSignaleringToDto} from "../../datatypes/Signalering"
import type {Form} from "../../datatypes/Form"
import {newAanvullendeInformatieForm} from "./VisueleInspectieContainer"
import {isFormValid, submitForm} from "../../util/ValidationUtil"
import {showErrorToast, showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import {createActionUpdateForm} from "../../actions/FormActions"
import {dispatchActions} from "../../util/DispatchUtil"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {createDubbeleTijdActions} from "../../util/OnderzoekUtil"
import type {AnnotatieAfbeelding} from "../../datatypes/AnnotatieAfbeelding"
import {createActionWijzigingenVerwerkt} from "../../actions/WijzigingenActions"
import {RootState, store} from "../../Store"
import {leesAfspraken} from "../../restclient/DaglijstRestclient"
import {showAmputatieWaarschuwingPopup, showWijzigingenPopup} from "../../util/PopupUtil"
import {checkOpBeeldenVanAmputatie, waarschuwingGecontroleerd} from "../../restclient/WerklijstRestclient"
import {createActionClearPopup} from "../../actions/PopupActions"
import type {DoorsnedeAfbeeldingen} from "../../datatypes/DoorsnedeAfbeeldingen"
import {newDoorsnedeAfbeeldingen} from "../../datatypes/DoorsnedeAfbeeldingen"
import type {SeAction} from "../../actions/SeAction"
import {createActionSetSignalering} from "../../actions/SignalerenActions"
import {Dispatch} from "redux"
import {navigateToDaglijst, navigateToOnderzoek} from "../../util/NavigationUtil"

export type SignalerenContainerProps = {
	client: Client;
	afspraak: Afspraak;
};

const mapStateToProps = (state: RootState, ownProps: SignalerenContainerProps): SignalerenViewStateProps => {
	const aanvullendeInformatieForm = getIfExists(state.formsByFormId, "onderzoek")
	const onderzoek: Onderzoek = getMandatory(state.onderzoekByAfspraakId, ownProps.afspraak.id)
	const afspraak: Afspraak = getMandatory(state.afsprakenById, ownProps.afspraak.id)
	const isEditable: boolean = !afspraak ? false : !afspraak.doorgevoerd
	return {
		afspraak: ownProps.afspraak,
		onderzoek: onderzoek,
		client: ownProps.client,
		magSignaleren: state.autorisatie.signaleren,
		heeftWijzigingen: state.heeftWijzigingen,
		isEditable: isEditable,
		online: state.online,
		signalering: getIfExists(state.signaleringByAfspraakId, ownProps.afspraak.id) || {
			heeftAfwijkingen: false,
			doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen(),
		},
		aanvullendeInformatieForm: aanvullendeInformatieForm ? aanvullendeInformatieForm : newAanvullendeInformatieForm(getMandatory(state.clientenById, state.navigation.clientId)),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): SignalerenViewDispatchProps => ({
	onVorige(client: Client, afspraak: Afspraak): void {
		if (store.getState().heeftWijzigingen) {
			showWijzigingenPopup(() => {
				leesAfspraken(store.getState().daglijstDatum, undefined, false)
				dispatch(createActionWijzigingenVerwerkt())
				navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
			}, dispatch)
		} else {
			navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
		}
	},
	onVolgende(afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan = false): void {
		dispatchActions(dispatch, createActionUpdateForm("onderzoek", submitForm(form)))

		if (!isFormValid(form)) {
			showErrorToast("De ingevoerde gegevens zijn niet valide.")
			return
		}

		if (afwijkingenGesignaleerdZonderAnnotaties(signalering)) {
			showErrorToast("Er zijn afwijkingen gesignaleerd, maar er zijn geen annotaties geplaatst.")
			return
		}

		checkOpBeeldenVanAmputatie(String(afspraak.uitnodigingsNr), onderzoek.amputatie).then((amputatieConflict) => {
			if (amputatieConflict) {
				showAmputatieWaarschuwingPopup(() => {
					waarschuwingGecontroleerd(afspraak.uitnodigingsNr, client.id)
					dispatchActions(dispatch, createActionClearPopup())
					opslaanEnNavigeren(dispatch, afspraak, client, onderzoek, signalering, form, alleenOpslaan)
				}, dispatch)
			} else {
				opslaanEnNavigeren(dispatch, afspraak, client, onderzoek, signalering, form, alleenOpslaan)
			}
		})
	},
	onInitializeForm(client: Client): void {
		dispatch(createActionUpdateForm("onderzoek", newAanvullendeInformatieForm(client)))
	},
})

const opslaanEnNavigeren = (dispatch: Dispatch, afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan: boolean): void => {

	if (!onderzoek.onvolledigOnderzoek && !onderzoek.onderbrokenOnderzoek) {
		onderzoek.status = "AFGEROND"
	}

	const signaleringAction = createActionSetSignalering(afspraak.id, mapSignaleringToDto(signalering))
	const afspraakAfrondenAction = createActionAfspraakAfronden(afspraak.id, signalering)
	const afspraakSignalerenOpslaanAction = createActionAfspraakSignalerenOpslaan(afspraak.id, signalering)
	const onderzoekOpslaanAction = createActionOnderzoekOpslaan(afspraak.id, onderzoek)
	const statusUpdateAction = createActionOnderzoekAfronden(afspraak.id, onderzoek.status === "ACTIEF" ? "AFGEROND" : onderzoek.status)
	const dubbeleTijdActions = createDubbeleTijdActions(afspraak, client)
	const heeftWijzigingenKlaarAction = createActionWijzigingenVerwerkt()

	const transactionActions = [onderzoekOpslaanAction, ...dubbeleTijdActions, signaleringAction, statusUpdateAction]
	const lokaleActions: Array<SeAction> = [heeftWijzigingenKlaarAction, statusUpdateAction]
	let navigeerNaarDaglijst: boolean

	if (!alleenOpslaan && afspraak.status === "SIGNALEREN") {
		transactionActions.push(afspraakAfrondenAction)
		lokaleActions.push(afspraakAfrondenAction)
		navigeerNaarDaglijst = true
	} else {
		transactionActions.push(afspraakSignalerenOpslaanAction)
		lokaleActions.push(afspraakSignalerenOpslaanAction)
		navigeerNaarDaglijst = false
	}

	putTransactionToScreenItCentraalPromise(afspraak, "SIGNALEREN_OPSLAAN", ...transactionActions).then(() => {
		showWijzigingenOpgeslagenToast()
	})

	dispatchActions(dispatch, ...lokaleActions)

	if (navigeerNaarDaglijst) {
		navigateToDaglijst(dispatch)
	}
}

const afwijkingenGesignaleerdZonderAnnotaties = (signalering: Signalering): boolean => {
	if (signalering.heeftAfwijkingen) {
		if (!signalering.doorsnedeAfbeeldingen) {
			return true
		} else {
			const doorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = signalering.doorsnedeAfbeeldingen
			return heeftLegeLijst(doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) && heeftLegeLijst(doorsnedeAfbeeldingen.linksVerticaleDoorsnede) && heeftLegeLijst(doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) && heeftLegeLijst(doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede)
		}
	}

	return false
}

const heeftLegeLijst = (afbeelding?: AnnotatieAfbeelding): boolean => {
	return (afbeelding && afbeelding.iconenById) ? afbeelding.iconenById.size < 1 : true
}

const SignalerenContainer = connect(mapStateToProps, mapDispatchToProps)(SignalerenView)
export default SignalerenContainer
