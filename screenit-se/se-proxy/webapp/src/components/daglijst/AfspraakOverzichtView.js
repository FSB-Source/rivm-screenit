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
import AfspraakLijstView from './AfspraakLijstView';
import type {Tijdslot} from '../../datatypes/Planning';
import {Afspraak} from '../../datatypes/Afspraak';
import {Col, Row} from 'reactstrap';
import {store} from '../../Store';
import BarcodeReader from 'react-barcode-reader';
import {GEFORCEERD_DAGLIJST_OPHALEN, vernieuwAfsprakenDaglijst} from '../../restclient/DaglijstRestclient';
import {createActionNavigateToClientgegevens, createActionNavigateToOnderzoek} from '../../actions/NavigationActions';
import {vandaagISO} from '../../util/DateUtil';

type AfspraakOverzichtProps = {
    nietAfgerondeTijdSlots: Array<Tijdslot>;
    afgerondeTijdSlots: Array<Tijdslot>;
    clienten: Map<number, Client>;
    daglijstDatum: string;
}

export default class AfspraakOverzichtView extends Component<AfspraakOverzichtProps> {

    constructor(props: AfspraakOverzichtProps) {
        super(props);
        this.props = props;
        if (store.getState().pendingUpdates !== null && store.getState().pendingUpdates.moetDaglijstNogVerversen) {
            vernieuwAfsprakenDaglijst();
        }
    }

    handleScan(gescandeData: string) {
        gescandeData = gescandeData.replace('http://', '').replace('https://', '');
        const afspraken = store.getState().afsprakenById;
        afspraken.forEach(function(afspraak: Afspraak) {
            if (String(afspraak.uitnodigingsNr) === gescandeData) {
                if (afspraak.vanafDatum !== vandaagISO()) {
                    return;
                }
                navigeerNaarClientAfspraak(afspraak);
                return;
            }

        });
    }

    handleError() {
    }

    render() {
        return (
            <div className="afspraaklijst">
                <BarcodeReader
                    onError={this.handleError}
                    onScan={this.handleScan}
                    minLength={9}
                />
                <Row>
                    <Col md={6}><h6>Gepland ({this.props.nietAfgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6></Col>
                    {this.props.daglijstDatum === vandaagISO() ? <Col md={6}><i className="fa fa-refresh float-right" id={'daglijst-refresh'} aria-hidden="true"
                                                                                onClick={() => {
                                                                                         vernieuwAfsprakenDaglijst(GEFORCEERD_DAGLIJST_OPHALEN);
                                                                             }}></i></Col> : null}
                </Row>
                <AfspraakLijstView afspraken={this.props.nietAfgerondeTijdSlots} clienten={this.props.clienten}
                                   emptyText='Er zijn geen geplande afspraken.'/>
                <h6>Afgerond ({this.props.afgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6>
                <AfspraakLijstView afspraken={this.props.afgerondeTijdSlots} clienten={this.props.clienten} emptyText='Er zijn geen afgeronde afspraken.'/>
            </div>
        );
    }
};

export const navigeerNaarClientAfspraak = (afspraak: Afspraak) => {
    if (!store.getState().autorisatie.onderzoeken) {
        store.dispatch(createActionNavigateToClientgegevens(afspraak.clientId, afspraak.id));
    } else {
        switch (afspraak.status) {
            case 'INGESCHREVEN':
                store.dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Vorige onderzoeken'));
                break;
            case 'ONDERZOEK':
                store.dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Visuele inspectie'));
                break;
            case 'SIGNALEREN':
                store.dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Signaleren'));
                break;
            default:
                store.dispatch(createActionNavigateToClientgegevens(afspraak.clientId, afspraak.id));
        }
    }
};
