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
import Paneel from '../generic/Paneel';
import PaneelNaam from '../generic/PaneelNaam';
import CheckboxValue from '../generic/CheckboxValue';

export type BezwaarProps = {
    afspraakId: number;
    bezwaarAangevraagd: boolean;
    isIngeschreven: boolean;
    bezwaarDoorgevroerdOpCentraal: boolean;

    onBezwaarAangevraagd: (afspraakId: number, bezwaar: boolean, isIngeschreven: boolean) => {};
}

export default class BezwaarView extends Component<BezwaarProps> {

    constructor(props: BezwaarProps) {
        super(props);
        this.props = props;
        this.bezwaarAanvragenDidChange.bind(this);
    }

    bezwaarAanvragenDidChange = (value: boolean) => {
        this.props.onBezwaarAangevraagd(this.props.afspraakId, value, this.props.isIngeschreven);
    };

    render() {
        return (
            <Paneel>
                <PaneelNaam titel={'Bezwaar'}/>
                <CheckboxValue label={'Bezwaar aanvragen'} checked={this.props.bezwaarAangevraagd} handleChange={this.bezwaarAanvragenDidChange}
                               disabled={this.props.bezwaarDoorgevroerdOpCentraal}/>
            </Paneel>
        );
    }
}
