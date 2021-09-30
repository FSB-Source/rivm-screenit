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
import AfwijkingenAfbeeldingenView from '../signaleren/AfwijkingenAfbeeldingenView';
import type {Lezing} from '../../../datatypes/Lezing';
import {Col} from 'reactstrap';
import type {AnnotatieIcoon} from '../../../datatypes/AnnotatieIcoon';
import BiradsWaardeView from './BiradsWaardeView';
import type {Aanzicht} from '../afbeelding/AnnotatieIcoonContainer';
import AfwijkingenContainerView from '../signaleren/AfwijkingenContainerView';
import Paneel from '../../generic/Paneel';

export type LezingBlokProps = {
    blokId: string;
    lezing: Lezing;
    colWidth: number;
}

export type LezingBlokState = {
    width: number,
    height: number;
    aanzichten: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>;
}

export default class LezingBlokView extends Component<LezingBlokProps, LezingBlokState> {

    linksHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    rechtsHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    linksVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;
    rechtsVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>;

    constructor(props: LezingBlokProps) {
        super(props);
        this.props = props;
        this.linksHorizontaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.linksHorizontaleDoorsnede ?
            this.props.lezing.lezingAanzichten.linksHorizontaleDoorsnede.iconenById :
            new Map();
        this.rechtsHorizontaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.rechtsHorizontaleDoorsnede ?
            this.props.lezing.lezingAanzichten.rechtsHorizontaleDoorsnede.iconenById :
            new Map();
        this.linksVerticaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.linksVerticaleDoorsnede ?
            this.props.lezing.lezingAanzichten.linksVerticaleDoorsnede.iconenById :
            new Map();
        this.rechtsVerticaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.rechtsVerticaleDoorsnede ?
            this.props.lezing.lezingAanzichten.rechtsVerticaleDoorsnede.iconenById :
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
        try {
            return (
                <Col md={this.props.colWidth} className={'px-2'}>
                    <div className={'vorig-onderzoek-lezing-blok'}>
                        <AfwijkingenContainerView blokId={this.props.blokId} lezingAanzichten={this.props.lezing.lezingAanzichten} isEditable={false}
                                                  amputatie={this.props.lezing.vorigOnderzoek.onderzoek.amputatie}/>
                        <BiradsWaardeView lezing={this.props.lezing}/>
                    </div>
                </Col>
            );
        }
        catch (exception) {
            console.warn('fout tijdens aanmaken van lezing: ' + exception.message);
            return (<Paneel className={'onderzoek-component paneel-shadow'}>Door een technische fout kan de vorige lezing niet correct worden ingeladen.</Paneel>);
        }
    }

};
