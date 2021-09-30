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
import IdentificatieContainer from './IdentificatieContainer';
import AdresContainer from './adres/AdresContainer';
import type {Client} from '../../datatypes/Client';
import TelefoonContainer from './TelefoonContainer';
import {Button, Row} from 'reactstrap';
import PaspoortContainer from '../paspoort/PaspoortContainer';
import BezwaarContainer from './BezwaarContainer';
import EmailContainer from './EmailContainer';
import type {Form} from '../../datatypes/Form';
import HuisartsContainer from './huisarts/HuisartsContainer';
import BarcodeReader from 'react-barcode-reader';
import {parseMRZ} from '../../util/mrzParser';
import {createActionKiesIdentificatienummer, createActionKiesIdentificatiesoort} from '../../actions/AfspraakActions';
import {store} from '../../Store';
import {createActionWijzigingIdentificatie} from '../../actions/WijzigingenActions';

type ClientgegevensProps = {
    client: Client,
    afspraak: Afspraak,
    heeftwijzigingen: boolean,
    onConfirm: (clientGegevensForm: Form, afspraak: Afspraak, client: Client, alleenOpslaan: ?boolean) => void,
    onInitializeForm: (client: Client, afspraak: Afspraak) => void,
    onConfirmUitschrijven: (client: Client, afspraak: Afspraak) => void,
    clientGegevensForm: Form,
}

export default class ClientgegevensView extends Component<ClientgegevensProps> {

    constructor(props: ClientgegevensProps) {
        super(props);
        this.props = props;
        this.handleScan = this.handleScan.bind(this);
        this.props.onInitializeForm(this.props.client, this.props.afspraak);
    }

    onConfirmClientgegevens = () => {
        this.props.onConfirm(this.props.clientGegevensForm, this.props.afspraak, this.props.client);
    };

    onOpslaanClientgegevens = () => {
        this.props.onConfirm(this.props.clientGegevensForm, this.props.afspraak, this.props.client, true);
    };

    onConfirmUitschrijven = () => {
        this.props.onConfirmUitschrijven(this.props.client, this.props.afspraak);
    };

    handleScan: Function = (gescandeData: string) => {
        const afspraakId = this.props.afspraak.id;
        const mrz = parseMRZ(gescandeData);
        if (mrz.id_type === 'RIJBEWIJS') {
            store.dispatch(createActionKiesIdentificatiesoort(afspraakId, mrz.id_type));
            store.dispatch(createActionKiesIdentificatienummer(afspraakId, mrz.id_nummer));
            store.dispatch(createActionWijzigingIdentificatie());
        }
    };

    handleError: Function = () => {

    };

    render() {
        let client: Client = this.props.client;
        return (
            <div className="inschrijven-scherm">
                <BarcodeReader
                    onError={this.handleError}
                    onScan={this.handleScan}
                    minLength={30}
                />
                <div className="tabpagina">
                    <Row>
                        <div className={'onderzoek-heading'}>
                            <h1 className="float-left">Cliëntgegevens</h1>
                            {this.props.afspraak.status === 'VERWACHT' ?
                                <Button className="btn-primary-se float-right" onClick={this.onConfirmClientgegevens}>Inschrijven</Button> :
                                this.props.afspraak.status === 'INGESCHREVEN' ?
                                    <Button className="float-right" color={'danger'} onClick={this.onConfirmUitschrijven}>Uitschrijven</Button>
                                    : null}
                            {this.props.heeftwijzigingen ? <Button className="btn-primary-se float-right" onClick={this.onOpslaanClientgegevens}>Opslaan</Button> : null}
                        </div>
                    </Row>
                    <Row>
                        <PaspoortContainer client={client} afspraak={this.props.afspraak}/>
                    </Row>
                    <Row>
                        <IdentificatieContainer afspraak={this.props.afspraak} disabled={this.props.afspraak.doorgevoerd}/>
                    </Row>
                    <Row>
                        <HuisartsContainer afspraak={this.props.afspraak} disabled={this.props.afspraak.doorgevoerd}/>
                    </Row>
                    <Row>
                        <AdresContainer clientId={client.id} adres={client.adres} tijdelijkGbaAdres={client.tijdelijkGbaAdres} tijdelijkAdres={client.tijdelijkAdres}
                                        disabled={this.props.afspraak.doorgevoerd}/>
                    </Row>
                    <Row>
                        <EmailContainer clientId={client.id} emailadres={client.emailadres} disabled={this.props.afspraak.doorgevoerd}/>
                    </Row>
                    <Row>
                        <TelefoonContainer telefoonnummer1={client.telefoonnummer1} telefoonnummer2={client.telefoonnummer2} disabled={this.props.afspraak.doorgevoerd}/>
                    </Row>
                    <Row>
                        <BezwaarContainer afspraakId={this.props.afspraak.id} bezwaarAangevraagd={this.props.afspraak.bezwaarAangevraagd}
                                          bezwaarDoorgevroerdOpCentraal={this.props.afspraak.bezwaarDoorgevroerdOpCentraal}
                                          isIngeschreven={this.props.afspraak.doorgevoerd}/>
                    </Row>
                </div>
            </div>
        );
    }
}
