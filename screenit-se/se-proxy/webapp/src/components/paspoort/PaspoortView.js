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
import type {Client} from '../../datatypes/Client';
import {datumFormaat} from '../../util/DateUtil';
import {Col, Row} from 'reactstrap';
import {postcodeMetSpatie} from '../../datatypes/Adres';
import LabelValue from '../generic/LabelValue';
import Paneel from '../generic/Paneel';
import PaneelNaam from '../generic/PaneelNaam';
import type {Afspraak} from '../../datatypes/Afspraak';
import {AANVULLENDE_BEELDEN_NODIG_SE} from '../../datatypes/OpschortenReden';

export type PaspoortProps = {
    client: Client;
    afspraak: Afspraak;
    tijdelijkAdresValue: string;
}

export default class PaspoortView extends Component<PaspoortProps> {

    constructor(props: PaspoortProps) {
        super(props);
        this.props = props;
    }

    render() {
        let client: Client = this.props.client;
        let afspraak: Afspraak = this.props.afspraak;
        return (
            <Paneel className="paspoort">
                <PaneelNaam className="paspoort-clientnaam" titel={client.voorletters + ' ' + client.aanspreekTussenvoegselEnAchternaam} children={
                    <div>
                        {this.props.client.inTehuis && <i className="fa fa-home px-1 py-1 float-right"/>}
                        {(!this.props.client.inTehuis && this.props.client.doelgroep === 'DUBBELE_TIJD') && <i className="fa fa-clock-o px-1 py-1 float-right"/>}
                        {this.props.client.doelgroep === 'MINDER_VALIDE' && <i className="fa fa-wheelchair px-1 py-1 float-right"/>}
                        {this.props.afspraak.eerderOnderbrokenInZelfdeRonde ? <i className="fa fa-step-forward px-1 py-1 float-right"/> : null}
                        {this.props.afspraak.eerdereOpschortenReden === AANVULLENDE_BEELDEN_NODIG_SE ? <i className="fa fa-plus px-1 py-1 float-right"/> : null}
                        {this.props.afspraak.geforceerd ? <i className="fa fa-plus px-1 py-1 float-right"/> : null}
                    </div>
                }/>

                <Row md={12}>
                    <Col sm={6}>
                        <LabelValue label="Burgerservicenummer" value={client.bsn}/>
                        <LabelValue label="Geboortedatum" value={datumFormaat(client.geboortedatum)}/>
                        <LabelValue label="Geboortenaam" value={(client.geboorteTussenvoegsel ? client.geboorteTussenvoegsel + ' ' : '') + client.geboorteAchternaam}/>
                        <LabelValue label="Uitnodigingsnummer" value={afspraak.uitnodigingsNr.toString()}/>
                        <LabelValue label="Opgeroepen" value={afspraak.aantalOproepen.toString()}/>
                    </Col>
                    <Col sm={6}>
                        <LabelValue label="Straat" value={client.adres.locatieBeschrijving}/>
                        <LabelValue label="Postcode" value={postcodeMetSpatie(client.adres.postcode)}/>
                        <LabelValue label="Plaats" value={client.adres.plaats}/>
                        <LabelValue label="Tijdelijk adres" value={this.props.tijdelijkAdresValue}/>
                        <LabelValue label="Opkomst" value={afspraak.aantalOpgekomen.toString()}/>
                    </Col>
                </Row>
            </Paneel>
        );
    }
}
