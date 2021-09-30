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
import inspectieAfbeelding from './visuele_inspectie_afbeelding.svg';
import type {AnnotatieIcoon, AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import {Card, CardBody, CardFooter, Row} from 'reactstrap';
import AnnotatiePaletView from '../afbeelding/AnnotatiePaletView';
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';
import AnnotatieIcoonContainer from '../afbeelding/AnnotatieIcoonContainer';
import type {Amputatie} from '../../../datatypes/Onderzoek';

const amputatieX = 33.5; 
const amputatieY = 45;

export type VisueleInspectieAfbeeldingProps = {
    containerId: string,
    afspraakId: number,
    width: number,
    height: number,
    iconenById: Map<number, AnnotatieIcoon>,
    paletIconen: Array<AnnotatieIcoonType>,
    afterLoad: () => void,
    isEditable?: boolean,
    noGutters?: boolean,
    amputatie: ?Amputatie,
}

export default class VisueleInspectieAfbeeldingView extends Component<VisueleInspectieAfbeeldingProps> {

    constructor(props: VisueleInspectieAfbeeldingProps) {
        super(props);
        this.props = props;
    }

    getMaxIconHeight() {
        return Math.max(...this.props.paletIconen.map((value: AnnotatieIcoonType) => {
            return getAfbeeldingByType(value).height;
        }));
    }

    render() {
        return (
            <Row className={'visuele-inspectie-row'} noGutters={this.props.noGutters}>
                <Card className={'visuele-inspectie-afbeelding-container'}
                      style={this.props.width <= 0 ? {minWidth: this.props.width + 'px'} : {minWidth: this.props.width + 'px', maxWidth: this.props.width + 'px'}}>
                    <CardBody className={'visuele-inspectie-afbeelding non-selectable'}>
                        <img id={'visuele-inspectie-afbeelding' + this.props.containerId} alt='visuele inspectie afbeelding' draggable="false" src={inspectieAfbeelding}
                             onLoadCapture={() => {
                                 this.props.afterLoad();
                             }}/>
                        {this.props.iconenById ? Array.from(this.props.iconenById.values()).map((icoon: AnnotatieIcoon) => {
                            return <AnnotatieIcoonContainer key={icoon.icoonId} afspraakId={this.props.afspraakId} icoon={icoon}
                                                            imageWidth={this.props.width}
                                                            imageHeight={this.props.height}
                                                            metTextbox={true}
                                                            isDraggable={this.props.isEditable}/>;
                        }) : null}
                        {this.props.amputatie ?
                            <AnnotatieIcoonContainer key={-1} afspraakId={this.props.afspraakId}
                                                     icoon={{
                                                         icoonId: -1,
                                                         positieX: this.props.amputatie === 'RECHTERBORST' ? amputatieX : 100 - amputatieX,
                                                         positieY: amputatieY,
                                                         type: 'AMPUTATIE',
                                                     }}
                                                     imageWidth={this.props.width}
                                                     imageHeight={this.props.height}
                                                     metTextbox={false}
                                                     isDraggable={this.props.isEditable}/> : null}
                    </CardBody>
                    {this.props.isEditable ?
                        <CardFooter className={'visuele-inspectie-afbeelding-palet'}>
                            <AnnotatiePaletView afspraakId={this.props.afspraakId} icoonList={this.props.paletIconen}
                                                imageWidth={this.props.width}
                                                imageHeight={this.props.height}
                                                paletHeight={this.getMaxIconHeight()}
                                                isEditable={this.props.isEditable}/>
                        </CardFooter>
                        : null}
                </Card>
            </Row>
        );
    }
}
