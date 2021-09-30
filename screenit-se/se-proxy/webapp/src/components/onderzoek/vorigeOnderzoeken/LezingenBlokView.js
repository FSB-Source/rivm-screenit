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
import {Col, Row} from 'reactstrap';
import type {VorigOnderzoek} from '../../../datatypes/VorigOnderzoek';
import Paneel from '../../generic/Paneel';
import type {Lezing} from '../../../datatypes/Lezing';
import LezingBlokView from './LezingBlokView';
import BiradsWaardeView from './BiradsWaardeView';

export type LezingenBlokProps = {
    vorigOnderzoek: VorigOnderzoek
}

export default class LezingenBlokView extends Component<LezingenBlokProps> {

    kolomWidth: number;

    constructor(props: LezingenBlokProps) {
        super(props);
        this.props = props;
        this.kolomWidth = (12 / (this.props.vorigOnderzoek.lezingen.length + 1));
    }

    render() {
        const verslagLezing = this.props.vorigOnderzoek.verslagLezing;
        try {
            return (
                <Paneel className={'onderzoek-component'}>
                    {
                        (this.props.vorigOnderzoek.uitslagGunstig !== null && this.bevatGeenEmptyLezing() && verslagLezing) ?
                            <div>
                                <Row noGutters>
                                    {this.props.vorigOnderzoek.lezingen.map((lezing: Lezing, index) => {
                                            return (
                                                <LezingBlokView key={index} blokId={this.props.vorigOnderzoek.uitnodigingsNr + '_' + index} lezing={lezing}
                                                                colWidth={this.kolomWidth}/>);
                                        },
                                    )}
                                    {this.props.vorigOnderzoek.uitslagGunstig === true ?
                                        <Col md={this.kolomWidth} className={'px-2 d-flex'}>
                                            <div className={'d-flex full-width full-width'}>
                                                <div className={'mt-auto full-width'}>
                                                    <BiradsWaardeView lezing={verslagLezing}/>
                                                </div>
                                            </div>
                                        </Col>
                                        :
                                        <LezingBlokView blokId={this.props.vorigOnderzoek.uitnodigingsNr + '_ID'} lezing={verslagLezing} colWidth={this.kolomWidth}/>

                                    }
                                </Row>
                                {this.props.vorigOnderzoek.nevenbevindingen ?
                                    <Row noGutters>
                                        <Col className={'px-2 col-md-4'}>
                                            <Paneel className={'onderzoek-component'}>
                                                <Row className={'mb-1'}>
                                                    <span className={'font-weight-bold'}>Gevonden nevenbevindingen: &nbsp;</span>
                                                    {this.props.vorigOnderzoek.nevenbevindingen}
                                                </Row>
                                                {this.props.vorigOnderzoek.nevenbevindingenOpmerkingen.length > 0 ?
                                                    <Row>
                                                        <span className={'font-weight-bold'}>Nevenbevindingen opmerking(en): </span>
                                                        <br/>
                                                        {this.props.vorigOnderzoek.nevenbevindingenOpmerkingen.map((value, key) => {
                                                            return (<Row className={'ondezoek-component'} noGutters key={key}>{value}</Row>);
                                                        })}
                                                    </Row> : null}
                                            </Paneel>
                                        </Col>
                                    </Row> : null}

                            </div>
                            :
                            <Row>
                                <Col md={12} className={'text-center font-weight-bold'}>{this.props.vorigOnderzoek.onbeoordeelbaar ?
                                    'Er was geen beoordeling mogelijk' :
                                    'Er is geen afgesloten beoordeling'}</Col>
                                <Col md={12} className={'text-center font-weight-bold'}>{this.props.vorigOnderzoek.onbeoordeelbaar ?
                                    'Er is geen uitslag omdat er geen beoordeling mogelijk was' :
                                    'Er is geen uitslag omdat er geen afgesloten beoordeling is'}</Col>
                            </Row>
                    }
                </Paneel>
            );
        } catch (exception) {
            console.warn('fout tijdens aanmaken van lezingen: ' + exception.message);
            return (
                <Paneel className={'onderzoek-component paneel-shadow'}>Door een technische fout kan de vorige onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt
                    het onderzoek normaal verder uitvoeren.</Paneel>);
        }
    };

    bevatGeenEmptyLezing = (): boolean => {
        return this.props.vorigOnderzoek.lezingen.filter(lezing => lezing === null).length < 1;
    };
};
