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
import AfwijkingenAfbeeldingenView from './AfwijkingenAfbeeldingenView';
import {Row} from 'reactstrap';
import type {AnnotatieIcoon} from '../../../datatypes/AnnotatieIcoon';
import type {Aanzicht} from '../afbeelding/AnnotatieIcoonContainer';
import type {DoorsnedeAfbeeldingen} from '../../../datatypes/DoorsnedeAfbeeldingen';
import type {Amputatie} from '../../../datatypes/Onderzoek';

export type AfwijkingenContainerProps = {
    afspraakId?: number;
    blokId: string;
    isEditable?: boolean,
    lezingAanzichten: DoorsnedeAfbeeldingen | null,
    amputatie: ?Amputatie,
}

export type AfwijkingenContainerState = {
    width: number,
    height: number;
    aanzichten: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>;
}

export default class AfwijkingenContainerView extends Component<AfwijkingenContainerProps, AfwijkingenContainerState> {

    linksHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    rechtsHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    linksVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    rechtsVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;

    constructor(props: AfwijkingenContainerProps) {
        super(props);
        this.props = props;
        this.linksHorizontaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.linksHorizontaleDoorsnede ?
            this.props.lezingAanzichten.linksHorizontaleDoorsnede.iconenById :
            new Map();
        this.rechtsHorizontaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.rechtsHorizontaleDoorsnede ?
            this.props.lezingAanzichten.rechtsHorizontaleDoorsnede.iconenById :
            new Map();
        this.linksVerticaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.linksVerticaleDoorsnede ?
            this.props.lezingAanzichten.linksVerticaleDoorsnede.iconenById :
            new Map();
        this.rechtsVerticaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.rechtsVerticaleDoorsnede ?
            this.props.lezingAanzichten.rechtsVerticaleDoorsnede.iconenById :
            new Map();
        this.updateParentState.bind(this);
    }

    updateParentState = () => {
        const element: HTMLElement | null = document ?
            document.getElementById('vorig-onderzoek-afwijking' + this.props.blokId) ? document.getElementById('vorig-onderzoek-afwijking' + this.props.blokId) : null : null;
        if (element) {
            this.setState({
                width: element.clientWidth,
                height: element.clientHeight,
                aanzichten: AfwijkingenAfbeeldingenView.getRenderedAanzichten(this.props.blokId),
            });
        }
    };

    render() {
        return (
            <Row noGutters id={'vorig-onderzoek-afwijking' + this.props.blokId}>
                <AfwijkingenAfbeeldingenView afspraakId={this.props.afspraakId}
                                             afwijkingenAfbeeldingId={this.props.blokId}
                                             iconenByIdRechtsVerticaal={this.rechtsVerticaleDoorsnedeIconen}
                                             iconenByIdLinksVerticaal={this.linksVerticaleDoorsnedeIconen}
                                             iconenByIdRechtsHorizontaal={this.rechtsHorizontaleDoorsnedeIconen}
                                             iconenByIdLinksHorizontaal={this.linksHorizontaleDoorsnedeIconen}
                                             onLoad={this.updateParentState}
                                             isEditable={this.props.isEditable}
                                             isNietVisueleInspectie={true}
                                             amputatie={this.props.amputatie}
                />
            </Row>
        );
    };

};
