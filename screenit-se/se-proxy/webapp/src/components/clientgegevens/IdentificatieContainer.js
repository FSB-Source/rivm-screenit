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

import {connect} from 'react-redux';
import IdentificatieView, {IDENTIFICATIE_FIELD_ID} from './IdentificatieView';
import type {Identificatiesoort} from '../../datatypes/Afspraak';
import {createActionKiesIdentificatienummer, createActionKiesIdentificatiesoort} from '../../actions/AfspraakActions';
import type {State} from '../../datatypes/State';
import {getMandatory} from '../../util/MapUtil';
import {createActionUpdateFormField} from '../../actions/FormActions';
import type {Form, FORM_FIELD_ID, FormField} from '../../datatypes/Form';
import {validateField} from '../../util/ValidationUtil';
import {dispatchActions} from '../../util/DispatchUtil';

const mapStateToProps = (state: State) => {

    return {
        clientGegevensForm: getMandatory(state.formsByFormId, 'clientgegevens'),
    };
};

const mapDispatchToProps = (dispatch, parentProps) => {
    return {
        onChooseSoort(identificatiesoort: Identificatiesoort, form: Form): void {
            this.updateField({identificatiesoort: identificatiesoort, identificatienummer: null}, IDENTIFICATIE_FIELD_ID, form, false);
            dispatchActions(dispatch, createActionKiesIdentificatiesoort(parentProps.afspraak.id, identificatiesoort));
            dispatchActions(dispatch, createActionKiesIdentificatienummer(parentProps.afspraak.id, null));
        },
        onChooseNummer(identificatienummer: ?string): void {
            dispatchActions(dispatch, createActionKiesIdentificatienummer(parentProps.afspraak.id, identificatienummer));
        },
        updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: ?boolean): void {
            const field: FormField<?string> = validateField(value, fieldId, form, showError);
            dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field));
        },
    };
};

const IdentificatieContainer = connect(mapStateToProps, mapDispatchToProps)(
    IdentificatieView);

export default IdentificatieContainer;
