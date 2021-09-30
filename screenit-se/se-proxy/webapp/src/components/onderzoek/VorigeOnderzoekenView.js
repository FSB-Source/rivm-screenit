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
import {vorigeOnderzoekenConfirmBtnKind} from './VorigeOnderzoekenContainer';
import PaspoortContainer from '../paspoort/PaspoortContainer';
import AutorisatieButton from '../generic/AutorisatieButton';
import VorigOnderzoekContainer from './vorigeOnderzoeken/VorigOnderzoekContainer';
import type {VorigOnderzoek} from '../../datatypes/VorigOnderzoek';
import Paneel from '../generic/Paneel';

export type VorigeOnderzoekenConfirmBtnKind = 'Onderzoek starten' | 'Volgende';

type VorigeOnderzoekenProps = {
    afspraak: Afspraak,
    client: Client,
    magOnderzoeken: boolean,
    gebruikersnaam: string,
    setHeeftOudeBeeldenOpgevraagd: () => void,
    onConfirm: (client: Client, afspraak: Afspraak) => {},
}

export default class VorigeOnderzoekenView extends Component<VorigeOnderzoekenProps> {

    constructor(props: VorigeOnderzoekenProps) {
        super(props);
        this.props = props;
    }

    render() {
        const client: Client = this.props.client;
        const afspraak: Afspraak = this.props.afspraak;
        const alleVorigeOnderzoekenAanwezig = Math.min(afspraak.aantalOpgekomen, 3) === this.props.client.vorigeOnderzoeken.length;
        return (
            <div className="tabpagina">
                <div className="row">
                    <div className="onderzoek-heading">
                        <h1 className="float-left">Vorige onderzoeken</h1>
                        <AutorisatieButton id="vorigeOnderzoekenButton" label={vorigeOnderzoekenConfirmBtnKind(this.props.afspraak)} heeftRecht={this.props.magOnderzoeken}
                                           rechtNaam={'Onderzoek starten op SE.'}
                                           onClick={() => {
                                               this.props.onConfirm(client, afspraak);
                                           }}>
                        </AutorisatieButton>
                    </div>
                </div>
                <div className="row">
                    <PaspoortContainer client={client} afspraak={afspraak}/>
                </div>
                {
                    alleVorigeOnderzoekenAanwezig ?
                        null :
                        <div className="row" key={'OnderzoekInformatieOntbreekt'}>
                            <Paneel className={'onderzoek-component paneel-shadow'}>
                                Door een technische fout kan de vorige onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt het onderzoek normaal verder uitvoeren
                            </Paneel>
                        </div>
                }
                {
                    this.props.client.vorigeOnderzoeken.map((vorigOnderzoek: VorigOnderzoek, index: number) => {
                        return (
                            <div className="row" key={index}>
                                <VorigOnderzoekContainer vorigOnderzoek={vorigOnderzoek} meestRecent={index === 0} client={this.props.client}
                                                         gebruikersnaam={this.props.gebruikersnaam} setHeeftOudeBeeldenOpgevraagd={this.props.setHeeftOudeBeeldenOpgevraagd}/>
                            </div>);
                    })
                }
            </div>
        );
    }
};
