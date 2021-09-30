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
import type {FormActions} from '../actions/FormActions';
import {UPDATE_FORM, UPDATE_FORM_FIELD} from '../actions/FormActions';
import {getMandatory} from '../util/MapUtil';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const formReducer = (stateSlice: Map<FORM_ID, Form> = new Map(), action: FormActions): Map<FORM_ID, Form> => {
    const result: Map<FORM_ID, Form> = new Map();
    const formFields: Map<FORM_FIELD_ID, FormField<*>> = new Map();

    switch (action.type) {
        case UPDATE_FORM:
            result.set(action.formId, action.form);
            break;
        case UPDATE_FORM_FIELD:
            formFields.set(action.fieldId, action.formField);
            result.set(action.formId, {
                formId: action.formId,
                fieldsById: new Map([...getMandatory(stateSlice, action.formId).fieldsById, ...formFields]),
                isSubmitted: getMandatory(stateSlice, action.formId).isSubmitted,
            });
            break;
        case CLEAR_CACHE:
            return new Map();
        default:
            return stateSlice;
    }
    return new Map([...stateSlice, ...result]);
};

export default formReducer;
