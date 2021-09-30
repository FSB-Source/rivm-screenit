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

import type {State} from '../../datatypes/State';
import {connect} from 'react-redux';
import type {ValidationInputValueProps} from './ValidationInputValue';
import ValidationInputValue from './ValidationInputValue';
import type {Form, FORM_FIELD_ID, FormField} from '../../datatypes/Form';
import {createActionUpdateFormField} from '../../actions/FormActions';
import {validateField} from '../../util/ValidationUtil';
import {getMandatory} from '../../util/MapUtil';
import {dispatchActions} from '../../util/DispatchUtil';

const mapStateToProps = (state: State, ownProps: ValidationInputValueProps<*>) => {
    return {
        form: getMandatory(state.formsByFormId, ownProps.fieldId.formId),
    };
};

const mapDispatchToProps = dispatch => ({
    updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: ?boolean): void {
        const field: FormField<?string> = validateField(value, fieldId, form, showError);
        dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field));
    },

});

const ValidationInputContainer = connect(mapStateToProps, mapDispatchToProps)(ValidationInputValue);

export default ValidationInputContainer;
