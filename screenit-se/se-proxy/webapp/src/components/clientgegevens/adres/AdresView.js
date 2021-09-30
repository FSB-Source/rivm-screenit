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
import {Button, Col, Row} from 'reactstrap';
import type {Adres} from '../../../datatypes/Adres';
import Paneel from '../../generic/Paneel';
import type {TijdelijkAdres} from '../../../datatypes/TijdelijkAdres';
import 'font-awesome/css/font-awesome.min.css';
import TijdelijkGbaAdresTitelView from './TijdelijkGbaAdresTitelView';
import AdresTitelView from './AdresTitelView';
import GbaAdresView from './GbaAdresView';
import TijdelijkAdresView from './TijdelijkAdresView';
import TijdelijkAdresWijzigenView from './TijdelijkAdresWijzigenView';

type AdresProps = {
    adres: Adres,
    tijdelijkGbaAdres: ?Adres,
    tijdelijkAdres: ?TijdelijkAdres,
    disabled: boolean,
    clientId: number,
}

type AdresState = {
    toonTijdelijkAdresWijzigenPopUp: boolean,
}

export default class AdresView extends Component<AdresProps, AdresState> {

    constructor(props: AdresProps) {
        super(props);
        this.props = props;
        this.state = {
            toonTijdelijkAdresWijzigenPopUp: false,
        };
        this.toggleTijdelijkAdresWijzigen = this.toggleTijdelijkAdresWijzigen.bind(this);
    };

    toggleTijdelijkAdresWijzigen = () => {
        this.setState({
            toonTijdelijkAdresWijzigenPopUp: !this.state.toonTijdelijkAdresWijzigenPopUp,
        });
    };

    render() {
        return (
            <Paneel>
                <Row>
                    <AdresTitelView adresTitel={'Adres'} className={'col-6'}/>
                    {this.props.tijdelijkGbaAdres ? <TijdelijkGbaAdresTitelView/> : null}
                    <AdresTitelView adresTitel={'Tijdelijk adres'} className={'col-4'}/>
                    <TijdelijkAdresWijzigenView
                        isOpen={this.state.toonTijdelijkAdresWijzigenPopUp}
                        toggle={this.toggleTijdelijkAdresWijzigen}
                        invoerenGedrukt={false}
                        clientId={this.props.clientId}/>
                    <div className={'col-2'}>
                        {!this.props.disabled && <Button className={'float-right btn-secondary-se'} onClick={this.toggleTijdelijkAdresWijzigen}>Wijzig tijdelijk adres</Button>}
                    </div>
                </Row>
                <Row>
                    <GbaAdresView adres={this.props.adres} className={'col-6'}/>
                    {this.props.tijdelijkGbaAdres ? <GbaAdresView adres={this.props.tijdelijkGbaAdres} className={'col-6'}/> : null}
                    {this.props.tijdelijkAdres ? <TijdelijkAdresView tijdelijkAdres={this.props.tijdelijkAdres} disabled={this.props.disabled} className={'col-4'}/> : <Col/>}
                    <div className={'col-2'}/>
                </Row>
            </Paneel>
        );
    }
}
;
