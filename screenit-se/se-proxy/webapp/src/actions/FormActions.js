/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import type {Form, FORM_FIELD_ID, FORM_ID, FormField} from '../datatypes/Form';

export type FormActions = UpdateFormFieldAction | UpdateFormAction;

export const UPDATE_FORM_FIELD = 'UPDATE_FORM_FIELD';
export type UpdateFormFieldAction = { type: 'UPDATE_FORM_FIELD', formId: FORM_ID, fieldId: FORM_FIELD_ID, formField: FormField<*> }
export const createActionUpdateFormField = (formId: FORM_ID, fieldId: FORM_FIELD_ID, formField: FormField<*>): UpdateFormFieldAction => ({
    type: UPDATE_FORM_FIELD,
    formId: formId,
    fieldId: fieldId,
    formField: formField,

});

export const UPDATE_FORM = 'UPDATE_FORM';
export type UpdateFormAction = { type: 'UPDATE_FORM', formId: FORM_ID, form: Form }
export const createActionUpdateForm = (formId: FORM_ID, form: Form): UpdateFormAction => ({
    type: UPDATE_FORM,
    formId: formId,
    form: form,
});
