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
import {Table} from 'reactstrap';
import Paneel from '../generic/Paneel';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import {getOnderzoekStatusCount} from '../../util/AfsprakenUtil';

type DagStatistiekenProps = {
    afspraken: Map<number, Afspraak>,
    onderzoeken: Map<number, Onderzoek>,
    afsprakenLength: number,
}

export default class DagStatistiekenView extends React.Component<DagStatistiekenProps> {

    constructor(props: DagStatistiekenProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <Paneel className='dagverslag-paneel'>
                <h6>
                    Dagstatistieken
                </h6>
                <Table className='table table-bordered table-condensed table-hover table-duo'>
                    <thead>
                    <tr>
                        <th>
                            Status
                        </th>
                        <th>
                        </th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                        <td>
                            Verwacht
                        </td>
                        <td>
                            {getOnderzoekStatusCount('VERWACHT', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Ingeschreven
                        </td>
                        <td>
                            {getOnderzoekStatusCount('INGESCHREVEN', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Onderzoek
                        </td>
                        <td>
                            {getOnderzoekStatusCount('ONDERZOEK', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Signaleren
                        </td>
                        <td>
                            {getOnderzoekStatusCount('SIGNALEREN', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Afgerond
                        </td>
                        <td>
                            {getOnderzoekStatusCount('AFGEROND', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Onderbroken
                        </td>
                        <td>
                            {getOnderzoekStatusCount('ONDERBROKEN', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <td>
                            Onvolledig
                        </td>
                        <td>
                            {getOnderzoekStatusCount('ONVOLLEDIG', this.props.onderzoeken, this.props.afspraken)}
                        </td>
                    </tr>
                    <tr>
                        <th>
                            Totaal
                        </th>
                        <th>
                            {this.props.afsprakenLength}
                        </th>
                    </tr>
                    </tbody>
                </Table>
            </Paneel>
        );
    }
}
