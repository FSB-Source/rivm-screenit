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
import {Col, Row} from 'reactstrap';

export type VorigOnderzoekUitklapBalkProps = {
    eersteBeeindigdeAfspraakOp: string;
    opgenklapt: boolean;
    uitslagGunstig: boolean | null;
    toggleOpengeklapt: () => void;
}

export default class VorigOnderzoekUitklapBalkView extends Component<VorigOnderzoekUitklapBalkProps> {

    constructor(props: VorigOnderzoekUitklapBalkProps) {
        super(props);
        this.props = props;
        this.props.toggleOpengeklapt.bind(this);
    }

    render() {
        return (
            <Row noGutters
                 className={(this.props.opgenklapt ? 'vorig-onderzoek-header' : 'vorig-onderzoek-dichtgeklapt vorig-onderzoek-header') + ' vorig-onderzoek p-2 px-4 clickable'}
                 onClick={this.props.toggleOpengeklapt}>
                <Col sm={1}>
                    {this.props.eersteBeeindigdeAfspraakOp ? this.props.eersteBeeindigdeAfspraakOp.substr(0, 4) : ''}
                </Col>
                {this.props.uitslagGunstig === false ?
                    <Col sm={1}>
                        <i className="fa fa-exclamation-triangle"/>
                    </Col>
                    : null
                }
            </Row>
        );
    }
};
