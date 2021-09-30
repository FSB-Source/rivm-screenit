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
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Client} from '../../datatypes/Client';
import PaspoortContainer from '../paspoort/PaspoortContainer';
import AutorisatieButton from '../generic/AutorisatieButton';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import {Col, Row} from 'reactstrap';
import AfwijkingenContainer from './signaleren/AfwijkingenContainer';
import type {Signalering} from '../../datatypes/Signalering';
import MbbSignaleringContainer from './inspectie/MbbSignaleringContainer';
import AanvullendeInformatieContainer from './inspectie/AanvullendeInformatieContainer';
import type {Form} from '../../datatypes/Form';

export type SignalerenAttrProps = {
    afspraak: Afspraak,
    onderzoek: Onderzoek,
    client: Client,
    magSignaleren: boolean,
    heeftWijzigingen: boolean,
    isEditable: boolean,
    signalering: Signalering,
    aanvullendeInformatieForm: Form,
    online: boolean,
}

export type SignalerenDispatchProps = {
    onVorige: (client: Client, afspraak: Afspraak) => {},
    onVolgende: (afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan: ?boolean) => {},
    onInitializeForm: (client: Client) => void,
}

export type SignalerenProps = SignalerenAttrProps & SignalerenDispatchProps;

export default class SignalerenView extends Component<SignalerenProps> {

    constructor(props: SignalerenProps) {
        super(props);
        this.props = props;
        this.props.onInitializeForm(this.props.client);
    }

    render() {
        let client: Client = this.props.client;
        let afspraak: Afspraak = this.props.afspraak;
        return (
            <div className="tabpagina">
                <div className="row">
                    <div className="onderzoek-heading">
                        <h1 className="float-left">Signaleren</h1>
                        {this.props.afspraak.status !== 'BEEINDIGD' ?
                            <AutorisatieButton id="afrondenButton"
                                               label={'Afronden'}
                                               heeftRecht={this.props.magSignaleren}
                                               online={this.props.online}
                                               rechtNaam={'Signaleren op SE'} onClick={() => {
                                this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.signalering, this.props.aanvullendeInformatieForm);
                            }}/> : null}
                        {this.props.heeftWijzigingen ? <AutorisatieButton id="opslaanButton"
                                                                          label={'Opslaan'}
                                                                          rechtNaam={'Signaleren op SE'}
                                                                          heeftRecht={this.props.magSignaleren}
                                                                          className="float-right btn btn-primary-se"
                                                                          onClick={() => {
                                                                              this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.signalering,
                                                                                  this.props.aanvullendeInformatieForm, true);
                                                                          }}>
                        </AutorisatieButton> : null}

                        <button className="float-right btn btn-primary-se" onClick={() => {
                            this.props.onVorige(client, afspraak);
                        }}>Vorige
                        </button>
                    </div>
                </div>
                <Row>
                    <PaspoortContainer client={client} afspraak={afspraak}/>
                </Row>
                <Row className="do-not-select">
                    <AfwijkingenContainer afspraakId={this.props.afspraak.id} signalering={this.props.signalering} isEditable={this.props.isEditable && this.props.magSignaleren}
                                          amputatie={this.props.onderzoek.amputatie}/>
                </Row>
                <Row className="onderzoek-row do-not-select">
                    <Col md={2}/>
                    <Col md={4}>
                        <MbbSignaleringContainer disabled={!this.props.magSignaleren}/>
                    </Col>
                    <Col md={4}>
                        <AanvullendeInformatieContainer disabled={!this.props.magSignaleren}/>
                    </Col>
                </Row>
            </div>
        );
    }
};
