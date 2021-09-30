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
import {daglijstStatusnaam} from '../../datatypes/Afspraak';
import type {Client} from '../../datatypes/Client';
import {datumFormaat} from '../../util/DateUtil';
import type {Onderzoekstatus} from '../../datatypes/Onderzoek';
import {AANVULLENDE_BEELDEN_NODIG_SE} from '../../datatypes/OpschortenReden';

export type AfspraakRijProps = {
    afspraak: Afspraak;
    onderzoekStatus: Onderzoekstatus | null;
    client: Client;
    klikbaar: boolean;
    magOnderzoeken: boolean;
    onRijKlik: (afspraak: Afspraak, client: Client, magOnderzoeken: boolean) => {};
}

export default class AfspraakRijView extends Component<AfspraakRijProps> {

    render() {
        const rijKlikAttributen = !this.props.klikbaar ? {} : {
            onClick: () => this.props.onRijKlik(this.props.afspraak, this.props.client, this.props.magOnderzoeken),
            style: {cursor: 'pointer'},
        };
        return (
            <tr className={this.className()} {...rijKlikAttributen} >
                <td>{this.props.afspraak.vanafTijd}
                    {this.props.client.inTehuis && <i className="fa fa-home px-1 py-1 float-right"/>}
                    {(!this.props.client.inTehuis && this.props.client.doelgroep === 'DUBBELE_TIJD') && <i className="fa fa-clock-o px-1 py-1 float-right"/>}
                    {this.props.client.doelgroep === 'MINDER_VALIDE' && <i className="fa fa-wheelchair px-1 py-1 float-right"/>}
                    {this.props.afspraak.eerderOnderbrokenInZelfdeRonde ? <i className="fa fa-step-forward px-1 py-1 float-right">
                        <span className="tooltiptext">Aan deze client is in de huidige screeningsronde een onderbroken onderzoek gekoppeld</span></i> : null}
                    {this.props.afspraak.eerdereOpschortenReden === AANVULLENDE_BEELDEN_NODIG_SE ?
                        <i className="fa fa-plus px-1 py-1 float-right">
                            <span className="tooltiptext">
                                <p>Bij een beoordeling in de huidige screeningsronde heeft een radioloog aangegeven aanvullende beelden nodig te hebben met opmerking:</p>
                                <p>{this.props.afspraak.eerdereOpschortenRedenTekst}</p>
                            </span>
                        </i> : null}
                    {this.props.afspraak.geforceerd ? <i className="fa fa-plus px-1 py-1 float-right">
                        <span className="tooltiptext">Geforceerde afspraak, let goed op welke beelden er nog gemaakt moeten worden</span></i> : null}

                </td>
                <td>{this.props.client.voorletters} {this.props.client.aanspreekTussenvoegselEnAchternaam}</td>
                <td>{datumFormaat(this.props.client.geboortedatum)}</td>
                <td>{this.props.client.bsn}</td>
                <td>
                    {this.props.afspraak.centralAvailable ? <i className="fa fa-camera-retro px-1 py-1 float-right">
                        <span className="tooltiptext">Beelden van onderzoek zijn beschikbaar in ScreenIT Centraal</span></i> : null}
                    {this.props.afspraak.doorgevoerd ? <i className="fa fa-lock px-1 py-1 float-right">
                        <span className="tooltiptext">Onderzoek is doorgevoerd en kan niet meer worden aangepast</span></i> : null}
                    {daglijstStatusnaam(this.props.afspraak, this.props.onderzoekStatus)}</td>
            </tr>
        );
    }

    className() {
        switch (this.props.afspraak.status) {
            case 'VERWACHT':
                return 'afspraakstatus-verwacht';
            case 'INGESCHREVEN':
                return 'afspraakstatus-ingeschreven';
            case 'ONDERZOEK':
                return 'afspraakstatus-onderzoek';
            case 'SIGNALEREN':
                return 'afspraakstatus-signaleren';
            case 'BEEINDIGD':
                if (this.props.onderzoekStatus !== null) {
                    switch (this.props.onderzoekStatus) {
                        case 'ONDERBROKEN':
                            return 'onderzoeksstatus-onderbroken';
                        case 'ONVOLLEDIG':
                            return 'onderzoeksstatus-onvolledig';
                        case 'AFGEROND':
                            return 'onderzoeksstatus-afgerond';
                        default:
                            throw Error('Ongeldige onderzoekstatus bij afgerond onderzoek.');
                    }
                }
                return '';
            default:
                return '';
        }
    }
}
