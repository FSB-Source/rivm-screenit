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
import ClientgegevensView from './ClientgegevensView';
import {createActionClientgegevensOpslaan, createActionInschrijven, createActionUitschrijven} from '../../actions/AfspraakActions';
import {createActionNavigateToDaglijst} from '../../actions/NavigationActions';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {State} from '../../datatypes/State';
import type {Client} from '../../datatypes/Client';
import {getIfExists, getMandatory} from '../../util/MapUtil';
import type {Form, FORM_FIELD_ID, FormField} from '../../datatypes/Form';
import {initialFormField} from '../../datatypes/Form';
import {createActionUpdateForm} from '../../actions/FormActions';
import {isFormValid, submitForm} from '../../util/ValidationUtil';
import {EMAIL_FIELD_ID} from './EmailView';
import {EmailValidator} from '../../validation/EmailValidator';
import {IDENTIFICATIE_FIELD_ID} from './IdentificatieView';
import {IdentificatieValidator} from '../../validation/IdentificatieValidator';
import {putTransactionToScreenItCentraalPromise} from '../../restclient/TransactionRestclient';
import {dispatchActions} from '../../util/DispatchUtil';
import {createActionSetEmailAdres} from '../../actions/ClientActions';
import {showErrorToast, showWijzigingenOpgeslagenToast} from '../../util/ToastUtil';
import {createActionWijzigingenVerwerkt, createActionWijzigingIdentificatie} from '../../actions/WijzigingenActions';
import {TelefoonnummerValidator} from '../../validation/TelefoonnummerValidator';
import {TELEFOON_1_FIELD_ID, TELEFOON_2_FIELD_ID} from './TelefoonView';

const mapStateToProps = (state: State) => {
    const afspraak: Afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId);
    const client: Client = getMandatory(state.clientenById, state.navigation.clientId);
    const clientGegevensForm: Form | null = getIfExists(state.formsByFormId, 'clientgegevens');
    return {
        client: client,
        afspraak: afspraak,
        heeftwijzigingen: state.heeftWijzigingen,
        clientGegevensForm: clientGegevensForm === null ? newClientGegevensForm(client, afspraak) : clientGegevensForm,
    };
};

export const newClientGegevensForm = (client: ?Client, afspraak: ?Afspraak): Form => {
    const fieldsMap: Map<FORM_FIELD_ID, FormField<*>> = new Map();
    fieldsMap.set(IDENTIFICATIE_FIELD_ID,
        initialFormField(afspraak ? {
                identificatienummer: afspraak.identificatienummer,
                identificatiesoort: afspraak.identificatiesoort,
            } : undefined, 'Identificatienummer',
            new IdentificatieValidator()));
    fieldsMap.set(EMAIL_FIELD_ID, initialFormField(client ? client.emailadres : undefined, 'E-mailadres', new EmailValidator()));
    fieldsMap.set(TELEFOON_1_FIELD_ID, initialFormField(client ? client.telefoonnummer1 : undefined, 'Telefoonnummer', new TelefoonnummerValidator()));
    fieldsMap.set(TELEFOON_2_FIELD_ID, initialFormField(client ? client.telefoonnummer2 : undefined, 'Mobiel nummer', new TelefoonnummerValidator()));
    return {
        formId: 'clientgegevens',
        fieldsById: fieldsMap,
        isSubmitted: false,
    };
};

const mapDispatchToProps = dispatch => {

    return {
        onConfirm(form: Form, afspraak: Afspraak, client: Client, alleenOpslaan: boolean = false): void {
            dispatchActions(dispatch, createActionUpdateForm('clientgegevens', submitForm(form)));
            if (!isFormValid(form) || (!afspraak.huisartsId && !afspraak.geenHuisartsOptie)) {
                showErrorToast('De ingevoerde cliëntgegevens zijn niet valide.');
                return;
            }
            let clientGegevensActions = [];
            clientGegevensActions.push(createActionSetEmailAdres(client.id, client.emailadres));
            clientGegevensActions.push(createActionWijzigingIdentificatie());

            if (!alleenOpslaan) {
                clientGegevensActions.push(createActionInschrijven(afspraak, client));
            } else {
                clientGegevensActions.push(createActionClientgegevensOpslaan(afspraak, client));
            }
            dispatchActions(dispatch, ...clientGegevensActions);
            putTransactionToScreenItCentraalPromise(afspraak, 'INSCHRIJFGEGEVENS_OPSLAAN', ...clientGegevensActions).then(() => {
                showWijzigingenOpgeslagenToast();
            });

            let lokaleActions = [];
            lokaleActions.push(createActionWijzigingenVerwerkt());
            if (!alleenOpslaan) {
                lokaleActions.push(createActionNavigateToDaglijst());
            }
            dispatchActions(dispatch, ...lokaleActions);
        },
        onConfirmUitschrijven(client: Client, afspraak: Afspraak): void {
            const uitschrijvenAction = createActionUitschrijven(afspraak.id);
            dispatchActions(dispatch, uitschrijvenAction, createActionNavigateToDaglijst());
            putTransactionToScreenItCentraalPromise(afspraak, 'UITSCHRIJVEN_CLIENT', uitschrijvenAction);
        },
        onInitializeForm(client: Client, afspraak: Afspraak): void {
            dispatch(createActionUpdateForm('clientgegevens', newClientGegevensForm(client, afspraak)));
        },
    };
};

const ClientgegevensContainer = connect(mapStateToProps, mapDispatchToProps)(ClientgegevensView);

export default ClientgegevensContainer;
