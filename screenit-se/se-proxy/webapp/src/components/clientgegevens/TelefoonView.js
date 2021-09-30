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
import Paneel from '../generic/Paneel';
import PaneelNaam from '../generic/PaneelNaam';
import type {Form, FORM_FIELD_ID} from '../../datatypes/Form';
import ValidationInputContainer from '../generic/ValidationInputContainer';

type TelefoonProps = {
    clientId: number;
    telefoonnummer1: string;
    telefoonnummer2: string;
    disabled: boolean;
    clientGegevensForm: Form;

    setTelefoon1: (clientId: number, telefoon: string) => void,
    setTelefoon2: (clientId: number, telefoon: string) => void

}

export const TELEFOON_1_FIELD_ID: FORM_FIELD_ID = {formId: 'clientgegevens', fieldId: 'telefoon1'};
export const TELEFOON_2_FIELD_ID: FORM_FIELD_ID = {formId: 'clientgegevens', fieldId: 'telefoon2'};

export default class TelefoonView extends Component<TelefoonProps> {

    constructor(props: TelefoonProps) {
        super(props);
        this.props = props;
        this.onTelefoon1Change = this.onTelefoon1Change.bind(this);
        this.onTelefoon2Change = this.onTelefoon2Change.bind(this);
    }

    onTelefoon1Change = (value: string): void => {
        this.props.setTelefoon1(this.props.clientId, value);
    };

    onTelefoon2Change = (value: string): void => {
        this.props.setTelefoon2(this.props.clientId, value);
    };

    render() {
        return (
            <Paneel>
                <PaneelNaam titel={'Telefoonnummer(s)'}/>
                <Row noGutters>
                    <Col md={2}>
                        Telefoonnummer
                    </Col>
                    <Col md={3}>
                        <ValidationInputContainer label={'Telefoonnummer'} placeholder={'Voer telefoonnummer in'} disabled={this.props.disabled}
                                                  value={this.props.telefoonnummer1 || ''} onChange={this.onTelefoon1Change}
                                                  fieldId={TELEFOON_1_FIELD_ID} maxLength={20}/>
                    </Col>
                    <Col md={1}/>
                    <Col md={2}>
                        Mobiel nummer
                    </Col>
                    <Col md={3}>
                        <ValidationInputContainer label={'Mobiel nummer'} placeholder={'Voer mobiel nummer in'} disabled={this.props.disabled}
                                                  value={this.props.telefoonnummer2 || ''} onChange={this.onTelefoon2Change}
                                                  fieldId={TELEFOON_2_FIELD_ID} maxLength={20}/>
                    </Col>
                </Row>
            </Paneel>
        );
    }
}
