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
import {Container, Row} from 'reactstrap';
import type {ErrorDto} from '../../datatypes/ErrorDto';

type ErrorProps = {
    error: ErrorDto
}

export default class ErrorView extends React.Component<ErrorProps> {

    constructor(props: ErrorProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <Container fluid>
                <Row>
                    <h2 className={'paneelnaam'}>Foutmelding</h2>
                </Row>
                <Row>
                    <p>
                        Er is een fout opgetreden in de applicatie met foutcode {this.props.error.errorReferentie}. Graag alleen deze code gebruiken tijdens correspondentie over deze fout.
                    </p>
                </Row>
                <Row>
                    <p>
                        Gelieve geen screenshot, maar alleen de foutcode doorgeven.
                    </p>
                </Row>
            </Container>
        );
    }

};
