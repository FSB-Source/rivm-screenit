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

import React from 'react';
import {Col, Row} from 'reactstrap';
import {Mammograaf} from '../../datatypes/Mammograaf';
import {gaOffline, gaOnline} from '../../restclient/TestRestclient';
import AlleenOnlineButton from '../generic/AlleenOnlineButton';
import Button from "reactstrap/lib/Button";

export type SessieViewProps = {
    aangemeld: boolean,
    gebruikersnaam: string,
    displayName: string,
    seCode: string,
    seNaam: string,
    huidigeMammograaf: ?Mammograaf,
    online: boolean,
    isTestOmgeving: boolean,

    afmelden: () => {},
}

export default class HeaderView extends React.Component<SessieViewProps> {

    constructor(props: SessieViewProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            this.props.aangemeld ?
                <Col md={10} className={'float-right gebruiker-view'}>
                    {
                        this.props.online === false ? <img className="geen-verbinding" src="images/geen-verbinding.png" alt="geen-verbinding"/> : null
                    }
                    {
                        this.props.isTestOmgeving ? this.props.online ?
                            <div className="col-2 offline-knop">
                                <input className="btn btn-danger" type="submit" value="Ga offline" onClick={() => {
                                    gaOffline();
                                }}/>
                            </div> :
                            <div className="col-2 offline-knop">
                                <input className="btn btn-success" type="submit" value="Ga online" onClick={() => {
                                    gaOnline();
                                }}/>
                            </div>
                            : null
                    }
                    <Row>
                        <Col md={5}>
                            <div className={'float-right'}>
                                {this.props.displayName}
                            </div>
                        </Col>
                        <Col md={3}>{this.props.seNaam}</Col>
                        <Col md={2}>{this.props.huidigeMammograaf ? this.props.huidigeMammograaf.aeTitle : ''}</Col>
                        <Col md={2}>
                            <Button id="afmeldButton"
                                                color={'link'} className={'btn mr-1 white-link-button btn-sm'}
                                                onClick={() => {
                                                    this.props.afmelden();
                                                }}> Afmelden </Button>
                        </Col>
                    </Row>
                </Col> :
                this.props.isTestOmgeving ? this.props.online ?
                    <Col md={10} className={'float-right gebruiker-view'}>
                        <div className="col-2 offline-knop">
                            <input className="btn btn-danger" type="submit" value="Ga offline" onClick={() => {
                                gaOffline();
                            }}/>
                        </div>
                    </Col> :
                    <Col md={10} className={'float-right gebruiker-view'}>
                        <div className="col-2 offline-knop">
                            <input className="btn btn-success" type="submit" value="Ga online" onClick={() => {
                                gaOnline();
                            }}/>
                        </div>
                    </Col>
                    : null
        );
    }
}
