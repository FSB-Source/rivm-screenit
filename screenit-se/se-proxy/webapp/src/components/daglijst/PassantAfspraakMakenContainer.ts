import {connect} from "react-redux"
import PassantAfspraakMakenView, {
	BSN_FIELD_ID,
	GEBOORTEDATUM_FIELD_ID,
	PassantAfspraakMakenViewDispatchProps,
	PassantAfspraakMakenViewStateProps,
} from "./PassantAfspraakMakenView"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import type {Form, FORM_FIELD_ID, FormField} from "../../datatypes/Form"
import {initialFormField} from "../../datatypes/Form"
import {GeboortedatumValidator} from "../../validation/GeboortedatumValidator"
import {BsnValidator} from "../../validation/BSNValidator"
import {createActionUpdateForm} from "../../actions/FormActions"
import {dispatchActions} from "../../util/DispatchUtil"
import {isFormValid, submitForm} from "../../util/ValidationUtil"
import {showErrorToast} from "../../util/ToastUtil"
import {readPassant} from "../../restclient/PassantZoekenRestClient"
import {datumInToekomst, datumInVerleden} from "../../util/DateUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): PassantAfspraakMakenViewStateProps => {
	const passantAfspraakMakenForm = getIfExists(state.formsByFormId, "passant_afspraak_maken")
	return {
		heeftInschrijvenRecht: state.autorisatie.inschrijven,
		online: state.online,
		datumNietVandaag: datumInVerleden(state.daglijstDatum) || datumInToekomst(state.daglijstDatum),
		passantAfspraakMakenForm: passantAfspraakMakenForm ? passantAfspraakMakenForm : newPassantAfspraakMakenForm(),
	}
}

export const newPassantAfspraakMakenForm = (): Form => {
	const fieldsMap: Map<FORM_FIELD_ID, FormField<any>> = new Map()
	fieldsMap.set(GEBOORTEDATUM_FIELD_ID, initialFormField("", "Geboortedatum", new GeboortedatumValidator()))
	fieldsMap.set(BSN_FIELD_ID, initialFormField("", "BSN", new BsnValidator()))
	return {
		formId: "passant_afspraak_maken",
		fieldsById: fieldsMap,
		isSubmitted: false,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): PassantAfspraakMakenViewDispatchProps => ({
	onInitializeForm(): void {
		dispatch(createActionUpdateForm("passant_afspraak_maken", newPassantAfspraakMakenForm()))
	},
	maakAfspraak(form: Form): void {
		dispatchActions(dispatch, createActionUpdateForm("passant_afspraak_maken", submitForm(form)))

		if (!isFormValid(form)) {
			showErrorToast("De ingevoerde cliëntgegevens zijn niet valide.")
			return
		}

		const geboortedatum: string = getMandatory(form.fieldsById, GEBOORTEDATUM_FIELD_ID).value
		const bsn: string = getMandatory(form.fieldsById, BSN_FIELD_ID).value
		readPassant(bsn, geboortedatum, dispatch)
	},
})

const PassantAfspraakMakenContainer = connect(mapStateToProps, mapDispatchToProps)(PassantAfspraakMakenView)
export default PassantAfspraakMakenContainer