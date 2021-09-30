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
import {postcodeMetSpatie} from '../../../datatypes/Adres';
import SingleValue from '../../generic/SingleValue';
import {datumFormaat} from '../../../util/DateUtil';
import type {TijdelijkAdres} from '../../../datatypes/TijdelijkAdres';
import {getLocatie} from '../../../datatypes/TijdelijkAdres';

type TijdelijkAdresProps = {
    tijdelijkAdres: TijdelijkAdres;
    disabled: boolean;
}

export default class TijdelijkAdresView extends Component<TijdelijkAdresProps> {

    constructor(props: TijdelijkAdresProps) {
        super(props);
        this.props = props;
    }

    render() {
        const tijdelijkAdres: TijdelijkAdres = this.props.tijdelijkAdres;
        return (
            <Col>
                <SingleValue value={getLocatie(tijdelijkAdres)}/>
                <SingleValue value={postcodeMetSpatie(tijdelijkAdres.postcode) + ' ' + (tijdelijkAdres.plaats || '')}/>
                {(tijdelijkAdres.startDatum || tijdelijkAdres.eindDatum) ?
                    <SingleValue value={datumFormaat(tijdelijkAdres.startDatum) + ' t/m ' + datumFormaat(tijdelijkAdres.eindDatum)}/>
                    : null}
            </Col>
        );
    }
}
