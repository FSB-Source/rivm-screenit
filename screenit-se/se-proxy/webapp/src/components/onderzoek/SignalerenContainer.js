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
import {createActionNavigateToDaglijst, createActionNavigateToOnderzoek} from '../../actions/NavigationActions';
import type {SignalerenAttrProps} from './SignalerenView';
import SignalerenView from './SignalerenView';
import {createActionAfspraakAfronden, createActionAfspraakSignalerenOpslaan} from '../../actions/AfspraakActions';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Client} from '../../datatypes/Client';
import type {State} from '../../datatypes/State';
import {getIfExists, getMandatory} from '../../util/MapUtil';
import {createActionOnderzoekAfronden, createActionOnderzoekOpslaan} from '../../actions/OnderzoekActions';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import type {Signalering} from '../../datatypes/Signalering';
import {mapSignaleringToDto} from '../../datatypes/Signalering';
import type {Form} from '../../datatypes/Form';
import {newAanvullendeInformatieForm} from './VisueleInspectieContainer';
import {isFormValid, submitForm} from '../../util/ValidationUtil';
import {showErrorToast, showWijzigingenOpgeslagenToast} from '../../util/ToastUtil';
import {createActionUpdateForm} from '../../actions/FormActions';
import {dispatchActions} from '../../util/DispatchUtil';
import {putTransactionToScreenItCentraalPromise} from '../../restclient/TransactionRestclient';
import {createDubbeleTijdActions} from '../../util/OnderzoekUtil';
import type {AnnotatieAfbeelding} from '../../datatypes/AnnotatieAfbeelding';
import {createActionWijzigingenVerwerkt} from '../../actions/WijzigingenActions';
import {store} from '../../Store';
import React from 'react';
import {leesAfspraken} from '../../restclient/DaglijstRestclient';
import {showAmputatieWaarschuwingPopup, showWijzigingenPopup} from '../../util/PopupUtil';
import {checkOpBeeldenVanAmputatie, waarschuwingGecontroleerd} from '../../restclient/WerklijstRestclient';
import {createActionClearPopup} from '../../actions/PopupActions';
import type {DoorsnedeAfbeeldingen} from '../../datatypes/DoorsnedeAfbeeldingen';
import {newDoorsnedeAfbeeldingen} from '../../datatypes/DoorsnedeAfbeeldingen';
import type {SeAction} from '../../actions/SeAction';
import {createActionSetSignalering} from '../../actions/SignalerenActions';

export type SignalerenContainerProps = {
    client: Client,
    afspraak: Afspraak
}

const mapStateToProps = (state: State, ownProps: SignalerenContainerProps): SignalerenAttrProps => {
    const aanvullendeInformatieForm: Form | null = getIfExists(state.formsByFormId, 'onderzoek');
    const onderzoek: Onderzoek = getMandatory(state.onderzoekByAfspraakId, ownProps.afspraak.id);
    const afspraak: Afspraak = getMandatory(state.afsprakenById, ownProps.afspraak.id);
    const isEditable: boolean = (afspraak === undefined || afspraak === null) ? false : !afspraak.doorgevoerd;
    return {
        afspraak: ownProps.afspraak,
        onderzoek: onderzoek,
        client: ownProps.client,
        magSignaleren: state.autorisatie.signaleren,
        heeftWijzigingen: state.heeftWijzigingen,
        isEditable: isEditable,
        online: state.online,
        signalering: getIfExists(state.signaleringByAfspraakId, ownProps.afspraak.id) || {heeftAfwijkingen: false, doorsnedeAfbeeldingen: newDoorsnedeAfbeeldingen()},
        aanvullendeInformatieForm: aanvullendeInformatieForm ? aanvullendeInformatieForm :
            newAanvullendeInformatieForm(getMandatory(state.clientenById, state.navigation.clientId)),
    };
};

const mapDispatchToProps = dispatch => {
    return {
        onVorige(client: Client, afspraak: Afspraak): void {
            if (store.getState().heeftWijzigingen) {
                showWijzigingenPopup(() => {
                    leesAfspraken(store.getState().daglijstDatum);
                    dispatch(createActionWijzigingenVerwerkt());
                    dispatch(createActionNavigateToOnderzoek(client.id, afspraak.id, 'Visuele inspectie'));
                }, dispatch);
            } else {
                dispatch(createActionNavigateToOnderzoek(client.id, afspraak.id, 'Visuele inspectie'));
            }
        },
        onVolgende(afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan: boolean): void {
            dispatchActions(dispatch, createActionUpdateForm('onderzoek', submitForm(form)));
            if (!isFormValid(form)) {
                showErrorToast('De ingevoerde gegevens zijn niet valide.');
                return;
            }
            if (afwijkingenGesignaleerdZonderAnnotaties(signalering)) {
                showErrorToast('Er zijn afwijkingen gesignaleerd, maar er zijn geen annotaties geplaatst.');
                return;
            }

            const view = this;
            checkOpBeeldenVanAmputatie(String(afspraak.uitnodigingsNr), onderzoek.amputatie).then(function(amputatieConflict) {
                if (amputatieConflict) {
                    showAmputatieWaarschuwingPopup(() => {
                        waarschuwingGecontroleerd(afspraak.uitnodigingsNr, client.id);
                        dispatchActions(dispatch, createActionClearPopup());
                        view.opslaanEnNavigeren(afspraak, client, onderzoek, signalering, form, alleenOpslaan);
                    }, dispatch);
                } else {
                    view.opslaanEnNavigeren(afspraak, client, onderzoek, signalering, form, alleenOpslaan);
                }
            });

        },
        opslaanEnNavigeren(afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan: boolean): void {
            const acties: [Array<SeAction>, Array<SeAction>] = maakSignalerenActions(afspraak, client, onderzoek, signalering, form, alleenOpslaan);
            putTransactionToScreenItCentraalPromise(afspraak, 'SIGNALEREN_OPSLAAN', ...acties[0]).then(() => {
                showWijzigingenOpgeslagenToast();
            });
            dispatchActions(dispatch, ...acties[1]);
        },
        onInitializeForm(client: Client): void {
            dispatch(createActionUpdateForm('onderzoek', newAanvullendeInformatieForm(client)));
        },
    };

};

const afwijkingenGesignaleerdZonderAnnotaties = (signalering: Signalering): boolean => {
    if (signalering.heeftAfwijkingen) {
        if (!signalering.doorsnedeAfbeeldingen) {
            return true;
        } else {
            const doorsnedeAfbeeldingen: DoorsnedeAfbeeldingen = signalering.doorsnedeAfbeeldingen;
            return heeftLegeLijst(doorsnedeAfbeeldingen.linksHorizontaleDoorsnede) && heeftLegeLijst(doorsnedeAfbeeldingen.linksVerticaleDoorsnede) &&
                heeftLegeLijst(doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede) && heeftLegeLijst(doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede);
        }
    }
    return false;
};

const heeftLegeLijst = (afbeelding: ?AnnotatieAfbeelding) => {
    return afbeelding ? afbeelding.iconenById.size < 1 : true;
};

const maakSignalerenActions = (
    afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan: boolean = false): [Array<SeAction>, Array<SeAction>] => {

    if (onderzoek.onvolledigOnderzoek === null && onderzoek.onderbrokenOnderzoek === null) {
        onderzoek.status = 'AFGEROND';
    }

    const signaleringAction = createActionSetSignalering(afspraak.id, mapSignaleringToDto(signalering));
    const afspraakAfrondenAction: SeAction = createActionAfspraakAfronden(afspraak.id, signalering);
    const afspraakSignalerenOpslaanAction: SeAction = createActionAfspraakSignalerenOpslaan(afspraak.id, signalering);
    const onderzoekOpslaanAction: SeAction = createActionOnderzoekOpslaan(afspraak.id, onderzoek);
    const statusUpdateAction = createActionOnderzoekAfronden(afspraak.id, onderzoek.status === 'ACTIEF' ? 'AFGEROND' : onderzoek.status);
    const dubbeleTijdActions: Array<SeAction> = createDubbeleTijdActions(afspraak, client);
    const heeftWijzigingenKlaarAction: SeAction = createActionWijzigingenVerwerkt();
    const transactionActions: Array<SeAction> = [onderzoekOpslaanAction, ...dubbeleTijdActions, signaleringAction, statusUpdateAction];
    const lokaleActions: Array<SeAction> = [heeftWijzigingenKlaarAction, statusUpdateAction];

    if (!alleenOpslaan && afspraak.status === 'SIGNALEREN') {
        transactionActions.push(afspraakAfrondenAction);
        lokaleActions.push(...[afspraakAfrondenAction, createActionNavigateToDaglijst()]);
    } else {
        transactionActions.push(afspraakSignalerenOpslaanAction);
        lokaleActions.push(afspraakSignalerenOpslaanAction);
    }
    return [transactionActions, lokaleActions];
};

const SignalerenContainer = connect(mapStateToProps, mapDispatchToProps)(SignalerenView);

export default SignalerenContainer;
