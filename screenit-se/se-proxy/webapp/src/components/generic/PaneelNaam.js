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

import * as React from 'react';
import {Col, Row} from 'reactstrap';

type PaneelNaamProps = {
    titel: string;
    className?: string,
    children?: React.Node
}

export default class PaneelNaam extends React.Component<PaneelNaamProps> {

    constructor(props: PaneelNaamProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <Row className={['paneelnaam mb-2', this.props.className].join(' ')}>
                <Col md={10}>{this.props.titel}</Col>
                <Col md={2}>{this.props.children}</Col>
            </Row>
        );
    }
}
