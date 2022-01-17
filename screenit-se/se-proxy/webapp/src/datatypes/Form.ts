import type {Validation} from "../validation/Validation"

export type FORM_ID = "clientgegevens" | "onderzoek" | "passant_afspraak_maken";
export type FORM_FIELD_ID = {
	formId: FORM_ID;
	fieldId: string;
};

export type FormField<T> = {
	isValid: boolean;
	errorMessage: string;
	showError: boolean;
	label: string;
	value: T;
	validator: Validation<T>;
};

export type Form = {
	formId: FORM_ID;
	fieldsById: Map<FORM_FIELD_ID, FormField<any>>;
	isSubmitted: boolean;
};

export const initialFormField = <T>(value: T, fieldLabel: string, validator: Validation<T>): FormField<any> => {
	return {
		isValid: value ? validator.isValid(value) : false,
		errorMessage: value ? validator.getErrorMessage(value, fieldLabel) : "",
		showError: false,
		label: fieldLabel,
		validator: validator,
		value: value,
	}
}