import type {Form, FORM_FIELD_ID, FormField} from "../datatypes/Form"
import {getMandatory} from "./MapUtil"

export const isFormValid = (form: Form): boolean => {
	let isValid = true
	for (const field of form.fieldsById.values()) {
		if (!field.validator.isValid(field.value)) {
			isValid = false
			break
		}
	}
	return isValid
}

export const submitForm = (form: Form): Form => {
	const formFields: Map<FORM_FIELD_ID, FormField<any>> = new Map()

	for (const key of form.fieldsById.keys()) {
		formFields.set(key, {
			...getMandatory(form.fieldsById, key),
			...{
				showError: true,
			},
		})
		let field: FormField<any> = getMandatory(formFields, key)
		if (!field.isValid && field.errorMessage === "") {
			field = validate(field, field.value)
		}
	}
	return {
		formId: form.formId,
		fieldsById: formFields,
		isSubmitted: true,
	}
}

const validate = (formField: FormField<any>, value: any): FormField<any> => {
	formField.isValid = formField.validator.isValid(value)
	formField.errorMessage = formField.validator.getErrorMessage(value, formField.label)
	return formField
}

export const validateField = (value: any, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): FormField<any> => {
	let formField: FormField<any> = {
		...getMandatory(form.fieldsById, fieldId),
	}
	formField.value = value
	formField = validate(formField, value)

	if (showError === true || showError === false) {
		formField.showError = showError
	}

	return formField
}