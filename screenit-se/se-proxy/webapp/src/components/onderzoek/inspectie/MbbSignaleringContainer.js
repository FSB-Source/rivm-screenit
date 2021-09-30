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
import type {State} from '../../../datatypes/State';
import MbbSignaleringView from './MbbSignaleringView';
import {getIfExists, getMandatory} from '../../../util/MapUtil';
import type {Onderzoek} from '../../../datatypes/Onderzoek';
import {
    createActionMaakAanvullendeInformatieOperatie,
    createActionMaakExtraMedewerker,
    createActionMaakMbbOpmerking,
    createActionMaakRadioloogOpmerking,
    createActionMaakRedenFotobespreking,
    createActionMaakSuboptimaleInsteltechniek,
    createActionOperatieLinks,
    createActionOperatieRechts,
} from '../../../actions/MBBSignaleringActions';
import type {Afspraak} from '../../../datatypes/Afspraak';
import {dispatchActions} from '../../../util/DispatchUtil';
import {store} from '../../../Store';
import type {SuboptimaleInsteltechniek} from '../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek';
import type {RedenFotobespreking} from '../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking';

type MbbSignalerenContainerProps = {
    disabled: boolean
}

const mapStateToProps = (state: State, ownProps: MbbSignalerenContainerProps) => {
    const disabled = ownProps.disabled;
    const onderzoek: Onderzoek | null = getIfExists(state.onderzoekByAfspraakId, state.navigation.afspraakId);
    const afspraak: Afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId);

    return {
        suboptimaleInsteltechniek: onderzoek ? onderzoek.suboptimaleInsteltechniek : null,
        redenFotobespreking: onderzoek ? onderzoek.redenFotobespreking : null,
        extraMedewerkerId: onderzoek ? onderzoek.extraMedewerkerId ? onderzoek.extraMedewerkerId : null : null,
        seGebruikers: store.getState().seGebruikers,
        ingelogdeGebruikerID: store.getState().session.instellingGebruikerId,
        opmerkingMbber: onderzoek ? onderzoek.opmerkingMbber : null,
        opmerkingVoorRadioloog: onderzoek ? onderzoek.opmerkingVoorRadioloog : null,
        operatieRechts: onderzoek ? onderzoek.operatieRechts : false,
        operatieLinks: onderzoek ? onderzoek.operatieLinks : false,
        aanvullendeInformatieOperatie: onderzoek ? onderzoek.aanvullendeInformatieOperatie : null,
        afspraakId: state.navigation.afspraakId,
        disabled: afspraak.doorgevoerd || disabled,
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        verwerkInsteltechniek: (afspraakId: number, suboptimaleInsteltechniek: ?SuboptimaleInsteltechniek) => {
            dispatchActions(dispatch, createActionMaakSuboptimaleInsteltechniek(afspraakId, suboptimaleInsteltechniek));
        },
        verwerkRedenFotobespreking: (afspraakId: number, redenFotobespreking: ?RedenFotobespreking) => {
            dispatchActions(dispatch, createActionMaakRedenFotobespreking(afspraakId, redenFotobespreking));
        },
        verwerkExtraMedewerkerId: (afspraakId: number, extraMedewerkerId: ?number) => {
            dispatchActions(dispatch, createActionMaakExtraMedewerker(afspraakId, extraMedewerkerId));
        },
        verwerkOpmerkingMbber: (afspraakId: number, opmerkingMbber: string) => {
            dispatchActions(dispatch, createActionMaakMbbOpmerking(afspraakId, opmerkingMbber));
        },
        verwerkOpmerkingVoorRadioloog: (afspraakId: number, opmerkingVoorRadioloog: string) => {
            dispatchActions(dispatch, createActionMaakRadioloogOpmerking(afspraakId, opmerkingVoorRadioloog));
        },
        verwerkOperatieRechtsChanged: (afspraakId: number, operatieRechts: boolean) => {
            dispatchActions(dispatch, createActionOperatieRechts(afspraakId, operatieRechts));
        },
        verwerkOperatieLinksChanged: (afspraakId: number, operatieLinks: boolean) => {
            dispatchActions(dispatch, createActionOperatieLinks(afspraakId, operatieLinks));
        },
        verwerkaanvullendeInformatieOperatie: (afspraakId: number, aanvullendeInformatieOperatie: string) => {
            dispatchActions(dispatch, createActionMaakAanvullendeInformatieOperatie(afspraakId, aanvullendeInformatieOperatie));
        },
    };
};

const MbbSignaleringContainer = connect(mapStateToProps, mapDispatchToProps)(MbbSignaleringView);

export default MbbSignaleringContainer;
