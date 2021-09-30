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

import React, {Component} from 'react';
import type {AnnotatieIcoon, AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import * as CoordinationCalculator from '../../../util/CoordinatenCalculator';
import VisueleInspectieAfbeeldingView from './VisueleInspectieAfbeeldingView';
import type {Amputatie} from '../../../datatypes/Onderzoek';

export type VisueleInspectieContainerViewProps = {
    containerId: string,
    afspraakId: number,
    iconenById: Map<number, AnnotatieIcoon>,
    paletIconen: Array<AnnotatieIcoonType>,
    isEditable?: boolean,
    noGutters?: boolean,
    amputatie: ?Amputatie,
}

export type VisueleInspectieContainerViewState = {
    width: number,
    height: number
}

export default class VisueleInspectieContainerView extends Component<VisueleInspectieContainerViewProps, VisueleInspectieContainerViewState> {

    constructor(props: VisueleInspectieContainerViewProps) {
        super(props);
        this.props = props;
        this.afterLoad.bind(this);
        this.state = {
            width: 0,
            height: 0,
        };
    }

    afterLoad = () => {
        this.setState({
            width: CoordinationCalculator.getWidth('visuele-inspectie-afbeelding' + this.props.containerId),
            height: CoordinationCalculator.getHeight('visuele-inspectie-afbeelding' + this.props.containerId),
        });
    };

    render() {
        return (
            <VisueleInspectieAfbeeldingView containerId={this.props.containerId} width={this.state.width} height={this.state.height} afspraakId={this.props.afspraakId}
                                            iconenById={this.props.iconenById} paletIconen={[]} noGutters={this.props.noGutters} afterLoad={this.afterLoad}
                                            isEditable={this.props.isEditable} amputatie={this.props.amputatie}/>
        );
    }
}
