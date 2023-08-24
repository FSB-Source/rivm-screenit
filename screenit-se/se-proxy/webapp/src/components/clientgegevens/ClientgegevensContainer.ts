import {connect} from "react-redux"
import ClientgegevensView, {ClientgegevensViewDispatchProps, ClientgegevensViewStateProps} from "./ClientgegevensView"
import {createActionClientgegevensOpslaan, createActionInschrijven, createActionUitschrijven} from "../../actions/AfspraakActions"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import type {Form, FORM_FIELD_ID, FormField} from "../../datatypes/Form"
import {initialFormField} from "../../datatypes/Form"
import {createActionUpdateForm} from "../../actions/FormActions"
import {isFormValid, submitForm} from "../../util/ValidationUtil"
import {EMAIL_FIELD_ID} from "./EmailView"
import {EmailValidator} from "../../validation/EmailValidator"
import {IDENTIFICATIE_FIELD_ID} from "./IdentificatieView"
import {IdentificatieValidator} from "../../validation/IdentificatieValidator"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {dispatchActions} from "../../util/DispatchUtil"
import {createActionSetEmailAdres} from "../../actions/ClientActions"
import {showErrorToast, showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import {createActionWijzigingenVerwerkt, createActionWijzigingIdentificatie} from "../../actions/WijzigingenActions"
import {MobielnummerValidator, TelefoonnummerValidator} from "../../validation/TelefoonnummerValidator"
import {TELEFOON_1_FIELD_ID, TELEFOON_2_FIELD_ID} from "./TelefoonView"
import {Dispatch} from "redux"
import {RootState} from "../../Store"
import {navigateToDaglijst} from "../../util/NavigationUtil"

const mapStateToProps = (state: RootState): ClientgegevensViewStateProps => {
	const afspraak: Afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)
	const client: Client = getMandatory(state.clientenById, state.navigation.clientId)
	const clientGegevensForm = getIfExists(state.formsByFormId, "clientgegevens")
	return {
		client: client,
		afspraak: afspraak,
		heeftwijzigingen: state.heeftWijzigingen,
		clientGegevensForm: !clientGegevensForm ? newClientGegevensForm(client, afspraak) : clientGegevensForm,
	}
}

export const newClientGegevensForm = (client: Client | undefined, afspraak: Afspraak | undefined): Form => {
	const fieldsMap: Map<FORM_FIELD_ID, FormField<any>> = new Map()
	fieldsMap.set(IDENTIFICATIE_FIELD_ID, initialFormField(afspraak ? {
		identificatienummer: afspraak.identificatienummer,
		identificatiesoort: afspraak.identificatiesoort,
	} : undefined, "Identificatienummer", new IdentificatieValidator()))
	fieldsMap.set(EMAIL_FIELD_ID, initialFormField(client?.emailadres, "E-mailadres", new EmailValidator()))
	fieldsMap.set(TELEFOON_1_FIELD_ID, initialFormField(client?.telefoonnummer1, "Mobiel nummer", new MobielnummerValidator()))
	fieldsMap.set(TELEFOON_2_FIELD_ID, initialFormField(client?.telefoonnummer2, "Extra telefoonnummer", new TelefoonnummerValidator()))
	return {
		formId: "clientgegevens",
		fieldsById: fieldsMap,
		isSubmitted: false,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): ClientgegevensViewDispatchProps => {
	return {
		onConfirm(form: Form, afspraak: Afspraak, client: Client, alleenOpslaan: boolean): void {
			dispatchActions(dispatch, createActionUpdateForm("clientgegevens", submitForm(form)))

			if (!isFormValid(form) || (!afspraak.huisartsId && !afspraak.geenHuisartsOptie)) {
				showErrorToast("De ingevoerde cliëntgegevens zijn niet valide.")
				return
			}

			const clientGegevensActions = []
			clientGegevensActions.push(createActionSetEmailAdres(client.id, client.emailadres))
			clientGegevensActions.push(createActionWijzigingIdentificatie())

			if (!alleenOpslaan) {
				clientGegevensActions.push(createActionInschrijven(afspraak, client))
			} else {
				clientGegevensActions.push(createActionClientgegevensOpslaan(afspraak, client))
			}

			dispatchActions(dispatch, ...clientGegevensActions)
			putTransactionToScreenItCentraalPromise(afspraak, "INSCHRIJFGEGEVENS_OPSLAAN", ...clientGegevensActions).then(() => {
				showWijzigingenOpgeslagenToast()
			})
			dispatch(createActionWijzigingenVerwerkt())

			if (!alleenOpslaan) {
				navigateToDaglijst(dispatch)
			}
		},

		onConfirmUitschrijven(client: Client, afspraak: Afspraak): void {
			const uitschrijvenAction = createActionUitschrijven(afspraak.id)
			dispatch(uitschrijvenAction)
			navigateToDaglijst(dispatch)
			putTransactionToScreenItCentraalPromise(afspraak, "UITSCHRIJVEN_CLIENT", uitschrijvenAction)
		},

		onInitializeForm(client: Client, afspraak: Afspraak): void {
			dispatch(createActionUpdateForm("clientgegevens", newClientGegevensForm(client, afspraak)))
		},

	}
}

const ClientgegevensContainer = connect(mapStateToProps, mapDispatchToProps)(ClientgegevensView)
export default ClientgegevensContainer
