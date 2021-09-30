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

import type {Validation} from '../validation/Validation';

export type FORM_ID = 'clientgegevens' | 'onderzoek' | 'passant_afspraak_maken';

export type FORM_FIELD_ID = { formId: FORM_ID, fieldId: string }

export type FormField<T> = {
    isValid: boolean,
    errorMessage: string,
    showError: boolean,
    label: string,
    value: T,
    validator: Validation<T>
}

export type Form = {
    formId: FORM_ID,
    fieldsById: Map<FORM_FIELD_ID, FormField<*>>,
    isSubmitted: boolean
};

export const initialFormField = <T>(value: T, fieldLabel: string, validator: Validation<T>): FormField<*> => {
    return {
        isValid: value ? validator.isValid(value) : false,
        errorMessage: value ? validator.getErrorMessage(value, fieldLabel) : '',
        showError: false,
        label: fieldLabel,
        validator: validator,
        value: value,
    };
};
