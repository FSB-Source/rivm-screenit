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
import type {VisueleInspectieAfbeeldingProps} from './VisueleInspectieAfbeeldingView';
import VisueleInspectieAfbeeldingView from './VisueleInspectieAfbeeldingView';
import type {State} from '../../../datatypes/State';
import type {AnnotatieAfbeelding} from '../../../datatypes/AnnotatieAfbeelding';
import {getIfExists, getMandatory} from '../../../util/MapUtil';
import {iconenLijst} from '../../../datatypes/IcoonAfbeelding';
import * as CoordinationCalculator from '../../../util/CoordinatenCalculator';
import {originalImageScaleFactor} from '../../../util/CoordinatenCalculator';
import {createActionVulVisueleInspectieAfbeeldingByAfspraakId} from '../../../actions/VisueleInspectieActions';
import {dispatchActions} from '../../../util/DispatchUtil';
import type {Client} from '../../../datatypes/Client';
import {Afspraak} from '../../../datatypes/Afspraak';
import {store} from '../../../Store';

const mapStateToProps = (state: State, ownProps: VisueleInspectieAfbeeldingProps) => {
    let visueleInspectieAfbeelding: ?AnnotatieAfbeelding = getIfExists(state.visueleInspectieAfbeeldingByAfspraakId, state.navigation.afspraakId);
    let client: Client = getMandatory(state.clientenById, state.navigation.clientId);
    let afspraak: Afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId);
    if (!visueleInspectieAfbeelding) {
        if (client.vorigeOnderzoeken && client.vorigeOnderzoeken[0]) {
            dispatchActions(store.dispatch, createActionVulVisueleInspectieAfbeeldingByAfspraakId(afspraak.id, client.vorigeOnderzoeken[0].visueleInspectieAfbeelding));
            visueleInspectieAfbeelding = client.vorigeOnderzoeken[0].visueleInspectieAfbeelding;
        } else {
            visueleInspectieAfbeelding = {afspraakId: state.navigation.afspraakId || 0, iconenById: new Map()};
        }
    }

    return {
        containerId: ownProps.containerId,
        width: ownProps.width || CoordinationCalculator.getWidth('visuele-inspectie-afbeelding' + ownProps.containerId),
        height: ownProps.height || (ownProps.width * originalImageScaleFactor),
        afspraakId: state.navigation.afspraakId,
        iconenById: visueleInspectieAfbeelding.iconenById,
        paletIconen: iconenLijst,
        afterLoad: ownProps.afterLoad ? ownProps.afterLoad : () => {
        },
        isEditable: ownProps.isEditable,
        amputatie: getMandatory(state.onderzoekByAfspraakId, state.navigation.afspraakId).amputatie,
    };
};

const mapDispatchToProps = () => {
    return {};
};

const VisueleInspectieAfbeeldingContainer = connect(mapStateToProps, mapDispatchToProps)(VisueleInspectieAfbeeldingView);

export default VisueleInspectieAfbeeldingContainer;
