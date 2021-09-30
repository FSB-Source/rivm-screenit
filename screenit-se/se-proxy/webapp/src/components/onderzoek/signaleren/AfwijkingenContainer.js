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
import AfwijkingenView from './AfwijkingenView';
import {signaleringPaletIconenLijst} from './SignaleringPalet';
import {getIfExists} from '../../../util/MapUtil';
import type {Signalering} from '../../../datatypes/Signalering';
import type {Amputatie} from '../../../datatypes/Onderzoek';

export type AfwijkingenContainerProps = {
    afspraakId: number,
    signalering: Signalering,
    isEditable: boolean,
    amputatie: Amputatie,
}

const mapStateToProps = (state: State, ownProps: AfwijkingenContainerProps) => {
    const signalering: Signalering | null = getIfExists(state.signaleringByAfspraakId, state.navigation.afspraakId);
    const isEditable = ownProps.isEditable;
    return {
        heeftAfwijkingen: ownProps.signalering.heeftAfwijkingen,
        magSignaleren: state.autorisatie.signaleren,
        afspraakId: ownProps.afspraakId,
        iconenByIdRechtsVerticaal: signalering && signalering.doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede ?
            signalering.doorsnedeAfbeeldingen.rechtsVerticaleDoorsnede.iconenById :
            new Map(),
        iconenByIdLinksVerticaal: signalering && signalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede ?
            signalering.doorsnedeAfbeeldingen.linksVerticaleDoorsnede.iconenById :
            new Map(),
        iconenByIdRechtsHorizontaal: signalering && signalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede ?
            signalering.doorsnedeAfbeeldingen.rechtsHorizontaleDoorsnede.iconenById :
            new Map(),
        iconenByIdLinksHorizontaal: signalering && signalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede ?
            signalering.doorsnedeAfbeeldingen.linksHorizontaleDoorsnede.iconenById :
            new Map(),
        paletIconen: signaleringPaletIconenLijst,
        isEditable: isEditable,
        amputatie: ownProps.amputatie,
    };
};

const mapDispatchToProps = () => {
    return {};
};

const AfwijkingenContainer = connect(mapStateToProps, mapDispatchToProps)(AfwijkingenView);

export default AfwijkingenContainer;
