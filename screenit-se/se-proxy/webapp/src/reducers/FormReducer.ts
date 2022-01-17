import type {Form, FORM_FIELD_ID, FORM_ID, FormField} from "../datatypes/Form"
import type {FormActions} from "../actions/FormActions"
import {UPDATE_FORM, UPDATE_FORM_FIELD} from "../actions/FormActions"
import {getMandatory} from "../util/MapUtil"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const FormReducer: Reducer<Map<FORM_ID, Form>, FormActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	const result: Map<FORM_ID, Form> = new Map()
	const formFields: Map<FORM_FIELD_ID, FormField<any>> = new Map()
	switch (action.type) {
		case UPDATE_FORM:
			result.set(action.formId, action.form)
			break
		case UPDATE_FORM_FIELD:
			formFields.set(action.fieldId, action.formField)
			result.set(action.formId, {
				formId: action.formId,
				fieldsById: new Map([...getMandatory(stateSlice, action.formId).fieldsById, ...formFields]),
				isSubmitted: getMandatory(stateSlice, action.formId).isSubmitted,
			})
			break
		case CLEAR_CACHE:
			return new Map()
		default:
			return stateSlice
	}
	return new Map([...stateSlice, ...result])
}

export default FormReducer