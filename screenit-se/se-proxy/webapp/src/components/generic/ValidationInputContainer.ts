import {connect} from "react-redux"
import ValidationInputValue, {ValidationInputValueDispatchProps, ValidationInputValueStateProps} from "./ValidationInputValue"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import {createActionUpdateFormField} from "../../actions/FormActions"
import {validateField} from "../../util/ValidationUtil"
import {getMandatory} from "../../util/MapUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"
import {Validation} from "../../validation/Validation"
import {InputType} from "reactstrap/es/Input"

export type ValidationInputContainerProps<T> = {
	id?: string;
	value: string;
	label?: string;
	transformValue?: (arg0: string) => T;
	fieldId: FORM_FIELD_ID;
	maxLength?: number;
	disabled: boolean;
	placeholder?: string;
	type?: InputType;
	color?: string;
	className?: string;
	validator?: Validation<T>;
	showRedErrorBackground?: boolean;
	onChange?: (inputValue: string) => void;
};

const mapStateToProps = (state: RootState, ownProps: ValidationInputContainerProps<any>): ValidationInputValueStateProps<any> => {
	return {
		...ownProps,
		form: getMandatory(state.formsByFormId, ownProps.fieldId.formId),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): ValidationInputValueDispatchProps<any> => ({
	updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): void {
		const field = validateField(value, fieldId, form, showError)
		dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field))
	},
})

const ValidationInputContainer = connect(mapStateToProps, mapDispatchToProps)(ValidationInputValue)
export default ValidationInputContainer