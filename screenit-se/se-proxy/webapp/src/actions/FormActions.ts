import type {Form, FORM_FIELD_ID, FORM_ID, FormField} from "../datatypes/Form"

export type FormActions = UpdateFormFieldAction | UpdateFormAction;
export const UPDATE_FORM_FIELD = "UPDATE_FORM_FIELD"
export type UpdateFormFieldAction = {
	type: "UPDATE_FORM_FIELD";
	formId: FORM_ID;
	fieldId: FORM_FIELD_ID;
	formField: FormField<any>;
};
export const createActionUpdateFormField = (formId: FORM_ID, fieldId: FORM_FIELD_ID, formField: FormField<any>): UpdateFormFieldAction => ({
	type: UPDATE_FORM_FIELD,
	formId: formId,
	fieldId: fieldId,
	formField: formField,
})
export const UPDATE_FORM = "UPDATE_FORM"
export type UpdateFormAction = {
	type: "UPDATE_FORM";
	formId: FORM_ID;
	form: Form;
};
export const createActionUpdateForm = (formId: FORM_ID, form: Form): UpdateFormAction => ({
	type: UPDATE_FORM,
	formId: formId,
	form: form,
})