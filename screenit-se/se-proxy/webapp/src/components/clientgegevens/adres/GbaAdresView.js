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
import {Col} from 'reactstrap';
import type {Adres} from '../../../datatypes/Adres';
import {postcodeMetSpatie} from '../../../datatypes/Adres';
import SingleValue from '../../generic/SingleValue';

type GbaAdresProps = {
    adres: Adres;
}

export default class GbaAdresView extends Component<GbaAdresProps> {

    constructor(props: GbaAdresProps) {
        super(props);
        this.props = props;
    }

    render() {
        const adres = this.props.adres;

        const postcodeEnPlaats: string = postcodeMetSpatie(adres.postcode) + ' ' + (adres.plaats ? adres.plaats : '');
        return (
            <Col>
                <SingleValue value={adres.locatieBeschrijving}/>
                <SingleValue value={postcodeEnPlaats}/>
            </Col>
        );
    }

}
