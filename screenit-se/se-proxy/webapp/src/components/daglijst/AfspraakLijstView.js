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
import {Afspraak, afspraakHash} from '../../datatypes/Afspraak';
import AfspraakRijContainer from './AfspraakRijContainer';
import {getMandatory} from '../../util/MapUtil';
import {Table} from 'reactstrap';
import type {Tijdslot} from '../../datatypes/Planning';
import {GeenScreeningBlok} from '../../datatypes/Planning';
import GeenScreeningBlokView from './GeenScreeningBlokView';

export type AfspraakLijstProps = {
    afspraken: Array<Tijdslot>;
    clienten: Map<number, Client>;
    emptyText: string
};

export default class AfspraakLijstView extends Component<AfspraakLijstProps> {

    constructor(props: AfspraakLijstProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            this.props.afspraken && this.props.afspraken.length > 0 ?
                <Table className="table table-bordered table-condensed table-hover" responsive={true}>
                    <thead>
                    <tr>
                        <th style={{width: '10%'}}>Tijd</th>
                        <th style={{width: '25%'}}>Cliënt</th>
                        <th style={{width: '25%'}}>Geboortedatum</th>
                        <th style={{width: '20%'}}>BSN</th>
                        <th style={{width: '20%'}}>Status</th>
                    </tr>
                    </thead>
                    <tbody>
                    {
                        this.props.afspraken.map((tijdslot: Tijdslot) => {

                            if (tijdslot instanceof Afspraak) {
                                return (<AfspraakRijContainer key={afspraakHash(tijdslot)} afspraak={tijdslot}
                                                              client={getMandatory(this.props.clienten, tijdslot.clientId)}/>);
                            }
                            if (tijdslot instanceof GeenScreeningBlok) {
                                return (<GeenScreeningBlokView key={tijdslot.vanafDatum + tijdslot.vanafTijd} geenScreeningBlok={tijdslot}/>);
                            }

                            return <div/>;
                        })
                    }
                    </tbody>
                </Table> : <p>{this.props.emptyText}</p>
        );
    }
};
