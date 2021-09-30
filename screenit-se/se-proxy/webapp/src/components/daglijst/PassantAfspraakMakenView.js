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
import {Col, Form, FormGroup, Label, Row} from 'reactstrap';
import AutorisatieButton from '../generic/AutorisatieButton';
import ValidationInputContainer from '../generic/ValidationInputContainer';
import type {FORM_FIELD_ID} from '../../datatypes/Form';
import {getIfExists, getMandatory} from '../../util/MapUtil';

type PassantAfspraakMakenViewProps = {
    heeftInschrijvenRecht: boolean,
    passantAfspraakMakenForm: Form,
    onInitializeForm: () => void,
    datumInVerleden: boolean,
    online: boolean,
    maakAfspraak: (passantAfspraakMakenForm: Form) => void,
    datumNietVandaag: boolean,
}

export const GEBOORTEDATUM_FIELD_ID: FORM_FIELD_ID = {formId: 'passant_afspraak_maken', fieldId: 'geboortedatum'};
export const BSN_FIELD_ID: FORM_FIELD_ID = {formId: 'passant_afspraak_maken', fieldId: 'bsn'};

export default class PassantAfspraakMakenView extends Component<PassantAfspraakMakenViewProps> {

    constructor(props: PassantAfspraakMakenViewProps) {
        super(props);
        this.props = props;
        this.props.onInitializeForm();
    }

    render() {
        if (this.props.datumNietVandaag) {
            return <div></div>;
        }
        return (
            <div className="afspraaklijst">

                <Form inline onSubmit={(e) => {
                    e.preventDefault();
                }}>
                    <Row noGutters>
                        <Col md={5}>
                            <FormGroup className="mb-2 mr-sm-2 mb-sm-0">
                                <Row noGutters>
                                    <Col md={4}>
                                        <div className="mr-sm-2">Geboortedatum</div>
                                    </Col>
                                    <Col md={6}>
                                        <ValidationInputContainer id={'geboortedatum'} type={'date'} label={'Geboortedatum'} placeholder={'Voer geboortedatum in'}
                                                                  disabled={!this.props.heeftInschrijvenRecht} className={'unstyled'}
                                                                  value={this.getGeboortedatum()} onChange={() => {
                                        }} fieldId={GEBOORTEDATUM_FIELD_ID} maxLength={10}/>
                                    </Col>
                                </Row>
                            </FormGroup>
                        </Col>
                        <Col md={5}>
                            <FormGroup className="mb-2 mr-sm-2 mb-sm-0">
                                <Row>
                                    <Col md={4}>
                                        <Label>Burgerservicenummer</Label>
                                    </Col>
                                    <Col md={7}>
                                        <ValidationInputContainer id={'bsn'} label={'BSN'} className={'unstyled'} placeholder={'Voer BSN in'}
                                                                  disabled={!this.props.heeftInschrijvenRecht}
                                                                  value={this.getBsn()} onChange={() => {
                                        }} fieldId={BSN_FIELD_ID} maxLength={9}/>
                                    </Col>
                                </Row>
                            </FormGroup>
                        </Col>
                        <Col md={2}>
                            <AutorisatieButton id="afspraakButton" label={'Maak afspraak'} online={this.props.online}
                                               heeftRecht={this.props.heeftInschrijvenRecht} rechtNaam={'inschrijven'} onClick={() => {
                                this.props.maakAfspraak(this.props.passantAfspraakMakenForm);
                            }}/>
                        </Col>
                    </Row>
                </Form>
            </div>
        );
    }

    getGeboortedatum() {
        return getIfExists(this.props.passantAfspraakMakenForm.fieldsById, GEBOORTEDATUM_FIELD_ID) ?
            getMandatory(this.props.passantAfspraakMakenForm.fieldsById, GEBOORTEDATUM_FIELD_ID).value : '';
    }

    getBsn() {
        return getIfExists(this.props.passantAfspraakMakenForm.fieldsById, BSN_FIELD_ID) ? getMandatory(this.props.passantAfspraakMakenForm.fieldsById, BSN_FIELD_ID).value : '';
    }
}
