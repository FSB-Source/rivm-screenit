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
import {readMammografenStatus} from '../../restclient/MammografenStatusRestclient';
import MammograafStatusContainer from './MammograafStatusContainer';
import type {ConnectieStatus, ConnectieStatusLevel} from '../../datatypes/connectiestatus/ConnectieStatus';
import {connectieStatusLevels, getMostCriticalStatusLevel} from '../../datatypes/connectiestatus/ConnectieStatus';
import type {MammografenStatus} from '../../datatypes/connectiestatus/MammografenStatus';
import {getTijdGeledenTekst, nuTijdUrenMinuten} from '../../util/DateUtil';
import type {ConnectieStatusItem} from './ConnectieStatusItem';
import {sendEmptyStudyMessageToIMS} from '../../util/ImsApiUtil';
import {Mammograaf} from '../../datatypes/Mammograaf';
import {Col, Container, Row} from 'reactstrap';

const REFRESH_CONNECTIE_STATUS_INTERVAL = 5 * 1000;

export type ConnectieStatusProps = {
    gebruikersNaam: string;
    seNaam: string;
    mammografen: Array<Mammograaf>;
    mammografenStatus: MammografenStatus;
    connectieStatus: ConnectieStatus;
}

export type ConnectieStatusState = {
    intervalId: ?IntervalID;
    refreshTimestamp: string;
}

export default class ConnectieStatusView extends Component<ConnectieStatusProps, ConnectieStatusState> implements ConnectieStatusItem {

    constructor(props: ConnectieStatusProps) {
        super(props);
        this.props = props;
        this.state = {
            intervalId: null,
            refreshTimestamp: '',
        };
        this.refresh.bind(this);
        this.forceImsUpdate.bind(this);
        this.getMammograafConnectieStatus.bind(this);
        this.getIMSConnectieStatus.bind(this);
        this.getStatusLevel.bind(this);
    }

    componentDidMount() {
        this.refresh();
        this.setState({...this.state, intervalId: setInterval(() => this.refresh(), REFRESH_CONNECTIE_STATUS_INTERVAL)});
    }

    componentWillUnmount(): * {
        if (this.state.intervalId) {
            clearInterval(this.state.intervalId);
        }
    }

    render() {
        return (
            <div className={'connectiestatus'}>

                <Container fluid={true}>

                    <Row className={'connectiestatus-header'}>
                        <Col className={'text-left'}>
                            <h1>Connectiestatus {this.props.seNaam}</h1>
                        </Col>
                    </Row>

                    <Container fluid={true}>
                        <Row className={'connectiestatus-statusbar ' + this.getStatusLevel().toLowerCase()}>
                            <Col className={'d-flex'}>
                                <p>{this.getStatusLevel() === 'FAULT' ?
                                    'Er zijn problemen geconstateerd, neem contact op met de helpdesk' :
                                    this.getStatusLevel() === 'WARN' ? 'Er zijn op dit moment waarschuwingen' : 'Er zijn op dit moment geen problemen geconstateerd'}</p>
                            </Col>
                            <Col className={'d-flex justify-content-end'}>
                                <p>{this.state.refreshTimestamp === '' ? 'Laden...' : 'Geüpdatet: ' + this.state.refreshTimestamp}</p>
                            </Col>
                        </Row>
                    </Container>

                    <Container fluid={true}>
                        <Row className={'connectiestatus-item'}>
                            <Col md={12}>
                                <Row>
                                    <i className={connectieStatusLevels[this.getMammograafConnectieStatus()]}/>
                                    <h1>Mammograaf verbinding</h1>
                                </Row>
                                {this.props.mammografenStatus.map(m => <MammograafStatusContainer key={m.aeTitle} mammograafStatus={m}/>)}
                            </Col>
                        </Row>

                        <Row className={'connectiestatus-item'}>
                            <Col>
                                <Row>
                                    <i className={connectieStatusLevels[this.getIMSConnectieStatus()]}/>
                                    <h1>Mammobridge <i>t.b.v. aansturing IDS7 huidige werkstation</i></h1>
                                </Row>
                                <Row className={'connectiestatus-item-data'}>
                                    {!this.props.connectieStatus.imsConnectieStatusTimestamp ? <p>Er is nog geen verbindingspoging met de Mammobridge afgerond</p> :
                                        <p>{this.getIMSConnectieStatus() === 'FAULT' ? 'Tijd laatste verbindingspoging: ' : 'Tijd laatst succesvol verstuurde bericht: '}
                                            <b>{this.props.connectieStatus.imsConnectieStatusTimestamp &&
                                            getTijdGeledenTekst(this.props.connectieStatus.imsConnectieStatusTimestamp)}</b></p>}
                                </Row>
                                <Row className={'connectiestatus-item-data'}>
                                    <button className="btn btn-primary-se" onClick={this.forceImsUpdate}>Forceer start IDS7 indien afgesloten</button>
                                </Row>
                            </Col>
                        </Row>
                    </Container>

                </Container>

            </div>
        );
    }

    refresh = (): void => {
        readMammografenStatus(this.props.mammografen).then(() => this.setState({refreshTimestamp: nuTijdUrenMinuten()}));
        this.forceUpdate();
    };

    forceImsUpdate = (): void => {
        sendEmptyStudyMessageToIMS(this.props.gebruikersNaam);
    };

    getStatusLevel = (): ConnectieStatusLevel => {
        return getMostCriticalStatusLevel([this.getMammograafConnectieStatus(), this.getIMSConnectieStatus()]);
    };

    getMammograafConnectieStatus = (): ConnectieStatusLevel => {
        return getMostCriticalStatusLevel(Array.from(this.props.connectieStatus.mammograafConnectieStatusByAeTitle.values()));
    };

    getIMSConnectieStatus = (): ConnectieStatusLevel => {
        return this.props.connectieStatus.imsConnectieStatus;
    };

}
