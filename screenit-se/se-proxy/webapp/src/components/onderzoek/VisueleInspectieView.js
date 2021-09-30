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
import {maakWerklijstItem} from './OnderzoekContainer';
import type {ClientWerklijstItem} from '../../datatypes/ClientWerklijstItem';
import VisueleInspectieAfbeeldingContainer from './inspectie/VisueleInspectieAfbeeldingContainer';
import PaspoortContainer from '../paspoort/PaspoortContainer';
import type {AnnotatieAfbeelding} from '../../datatypes/AnnotatieAfbeelding';
import {Col, Row} from 'reactstrap';
import MbbSignaleringContainer from './inspectie/MbbSignaleringContainer';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import AanvullendeInformatieContainer from './inspectie/AanvullendeInformatieContainer';
import type {Form} from '../../datatypes/Form';
import {meldingAfgeslotenProcedure} from '../../restclient/WerklijstRestclient';

export type VisueleInspectieProps = {
    afspraak: Afspraak,
    client: Client,
    onderzoek: Onderzoek,
    afbeelding: AnnotatieAfbeelding,
    magSignaleren: boolean,
    aanvullendeInformatieForm: Form,
    werkstationcode: string,
    heeftWijzigingen: boolean,
    aeTitle: ?string,
    onVorige: (afspraak: Afspraak) => {},
    onVolgende: (afspraak: Afspraak, client: Client, onderzoek: Onderzoek, afbeelding: AnnotatieAfbeelding, magSignaleren: boolean, form: Form, alleenOpslaan: ?boolean) => {},
    toevoegenAanWerklijst: (werklijstItem: ClientWerklijstItem) => {},
    verwijderVanWerklijst: (aeTitle: string) => {},
    onInitializeForm: (client: Client) => void,
    isEditable: boolean,
}

type VisueleInspectieState = {
    width: number;
}

export default class VisueleInspectieView extends Component<VisueleInspectieProps, VisueleInspectieState> {

    constructor(props: VisueleInspectieProps) {
        super(props);
        this.props = props;
        this.state = {
            width: 0,
        };
        if (this.props.aeTitle && !this.props.afspraak.doorgevoerd) {
            this.props.toevoegenAanWerklijst(maakWerklijstItem(this.props.afspraak, this.props.client, this.props.aeTitle));
        }
        this.props.onInitializeForm(this.props.client);
    }

    componentWillUnmount() {
        if (this.props.aeTitle) {
            this.props.verwijderVanWerklijst(this.props.aeTitle);
        }
    }

    componentDidUpdate() {
        if (this.props.aeTitle && !this.props.afspraak.doorgevoerd) {
            this.props.toevoegenAanWerklijst(maakWerklijstItem(this.props.afspraak, this.props.client, this.props.aeTitle));
        }
    }

    componentDidMount() {
        const element: HTMLElement | null = document ? document.getElementById('col-afbeelding-container') ? document.getElementById('col-afbeelding-container') : null : null;
        if (element) {
            this.setState({width: element.clientWidth - parseFloat(window.getComputedStyle(element).paddingRight)});
        }
    }

    render() {
        let client: Client = this.props.client;
        let afspraak: Afspraak = this.props.afspraak;
        return (
            <div className="tabpagina">
                <Row>
                    <div className="onderzoek-heading">
                        <h1 className="float-left">Visuele inspectie</h1>
                        <button className="float-right btn btn-primary-se"
                                onClick={() => {
                                    meldingAfgeslotenProcedure(client.id, this.props.afspraak.uitnodigingsNr);
                                    this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.afbeelding, this.props.magSignaleren,
                                        this.props.aanvullendeInformatieForm);
                                }}>
                            {this.props.afspraak.status === 'ONDERZOEK' ? 'Signaleren' : 'Volgende'}
                        </button>
                        {this.props.heeftWijzigingen ? <button
                            className="float-right btn btn-primary-se"
                            onClick={() => {
                                this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.afbeelding, this.props.magSignaleren,
                                    this.props.aanvullendeInformatieForm, true);
                            }}>
                            Opslaan
                        </button> : null}
                        <button className="float-right btn btn-primary-se" onClick={() => {
                            this.props.onVorige(afspraak);
                        }}>Vorige
                        </button>
                    </div>
                </Row>
                <Row>
                    <PaspoortContainer client={client} afspraak={afspraak}/>
                </Row>
                <Row className="onderzoek-row do-not-select">
                    <Col id={'col-afbeelding-container'} md={5}>
                        <VisueleInspectieAfbeeldingContainer width={this.state.width} isEditable={this.props.isEditable}/>
                    </Col>
                    <Col md={3}>
                        <MbbSignaleringContainer/>
                    </Col>
                    <Col md={4} className={'pr-0'}>
                        <AanvullendeInformatieContainer/>
                    </Col>
                </Row>
            </div>
        );
    }
};
