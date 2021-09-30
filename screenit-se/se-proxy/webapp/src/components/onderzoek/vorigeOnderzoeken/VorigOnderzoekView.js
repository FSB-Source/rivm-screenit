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
import type {VorigOnderzoek} from '../../../datatypes/VorigOnderzoek';
import {Button} from 'reactstrap';
import type {Client} from '../../../datatypes/Client';
import VorigOnderzoekUitklapBalkView from './VorigOnderzoekUitklapBalk';
import LezingenBlokView from './LezingenBlokView';
import OnderzoekBlokView from './OnderzoekBlokView';
import Paneel from '../../generic/Paneel';

export type VorigOnderzoekProps = {
    vorigOnderzoek: VorigOnderzoek;
    meestRecent: boolean;
    client: Client,
    gebruikersnaam: string,
    setHeeftOudeBeeldenOpgevraagd: () => void,

    vorigeOnderzoekOphalen: (clientId: number, uitnodigingsNr: number, gebruikersnaam: string, bsn: string) => {}
}

type VorigOnderzoekState = {
    opengeklapt: boolean;
}

export default class VorigOnderzoekView extends Component<VorigOnderzoekProps, VorigOnderzoekState> {

    constructor(props: VorigOnderzoekProps) {
        super(props);
        this.props = props;
        this.props.setHeeftOudeBeeldenOpgevraagd.bind(this);
        this.state = {
            opengeklapt: props.meestRecent,
        };
    }

    toggleOpengeklapt = () => {
        this.setState({opengeklapt: !this.state.opengeklapt});
    };

    render() {
        try {
            return (this.state.opengeklapt ?
                    <div className="vorig-onderzoek-opengeklapt vorig-onderzoek">
                        <VorigOnderzoekUitklapBalkView eersteBeeindigdeAfspraakOp={this.props.vorigOnderzoek.eersteBeeindigdeAfspraakOp} toggleOpengeklapt={this.toggleOpengeklapt}
                                                       opgenklapt={this.state.opengeklapt} uitslagGunstig={this.props.vorigOnderzoek.uitslagGunstig}/>
                        <div className="vorig-onderzoek p-2 px-4">
                            <OnderzoekBlokView vorigOnderzoek={this.props.vorigOnderzoek}/>
                            <LezingenBlokView vorigOnderzoek={this.props.vorigOnderzoek}/>
                            {!this.props.meestRecent && this.props.vorigOnderzoek.beeldenBeschikbaar ?
                                <Button color={'link'} className={'float-right gray-link'} onClick={() => {
                                    this.props.vorigeOnderzoekOphalen(this.props.client.id, this.props.vorigOnderzoek.uitnodigingsNr, this.props.gebruikersnaam,
                                        this.props.client.bsn);
                                    this.props.setHeeftOudeBeeldenOpgevraagd();
                                }}>Ophalen beelden</Button>
                                : null
                            }
                        </div>
                    </div>
                    :
                    <VorigOnderzoekUitklapBalkView eersteBeeindigdeAfspraakOp={this.props.vorigOnderzoek.eersteBeeindigdeAfspraakOp} toggleOpengeklapt={this.toggleOpengeklapt}
                                                   opgenklapt={this.state.opengeklapt} uitslagGunstig={this.props.vorigOnderzoek.uitslagGunstig}/>
            );
        } catch (exception) {
            console.warn('fout tijdens aanmaken van vorig onderzoek view: ' + exception.message);
            return (
                <Paneel className={'onderzoek-component paneel-shadow'}>Door een technische fout kan de vorige onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt het onderzoek
                    normaal verder uitvoeren.</Paneel>
            );
        }
    }
}
