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
import {Card, CardBody, Input, Label, Row} from 'reactstrap';
import DropdownValue from '../../generic/DropdownValue';
import type {SuboptimaleInsteltechniek} from '../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek';
import {getSuboptimaleInsteltechniekBeschrijving} from '../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek';
import TextAreaValue from '../../generic/TextAreaValue';
import type {RedenFotobespreking} from '../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking';
import {getRedenFotobesprekingString} from '../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking';
import {undefinedOrNull} from '../../../util/ValidationUtil';
import {zelfdeTekst} from '../../../util/StringUtil';

export type MbbSignaleringProps = {
    afspraakId: number,
    suboptimaleInsteltechniek: ?SuboptimaleInsteltechniek,
    redenFotobespreking: ?RedenFotobespreking,
    extraMedewerkerId: ?number,
    seGebruikers: Map<number, string>,
    ingelogdeGebruikerID: number,
    opmerkingMbber: ?string,
    opmerkingVoorRadioloog: ?string,
    operatieRechts: boolean,
    operatieLinks: boolean,
    aanvullendeInformatieOperatie: ?string,
    disabled: false,

    verwerkInsteltechniek: (afspraakId: number, suboptimaleInsteltechniek: ?string) => void,
    verwerkRedenFotobespreking: (afspraakId: number, redenFotobespreking: ?string) => void,
    verwerkExtraMedewerkerId: (afspraakId: number, extraMedewerkerId: ?number) => void,
    verwerkOpmerkingMbber: (afspraakId: number, opmerking: string) => void,
    verwerkOpmerkingVoorRadioloog: (afspraakId: number, opmerking: string) => void,
    verwerkOperatieRechtsChanged: (afspraakId: number, operatieR: boolean) => void,
    verwerkOperatieLinksChanged: (afspraakId: number, operatieL: boolean) => void,
    verwerkaanvullendeInformatieOperatie: (afspraakId: number, value: string) => void
}

const optionInsteltechniek: Array<SuboptimaleInsteltechniek> = [
    'FYSIEK_BEPERKT',
    'MOBIEL_BEPERKT',
    'MOEILIJK_TE_POSITIONEREN'];

const optionFotobespreking: Array<RedenFotobespreking> = [
    'ARCHITECTUURVERSTORING',
    'ASYMMETRIE',
    'CALCIFICATIES',
    'MASSA',
    'INSTELTECHNIEK',
    'OVERIG'];

const maxOpmerkingLengte: number = 255;

export default class MbbSignaleringView extends Component<MbbSignaleringProps> {

    constructor(props: MbbSignaleringProps) {
        super(props);
        this.props = props;
        this.suboptimaleInsteltechniekDidChange.bind(this);
        this.redenFotoBesprekingDidChange.bind(this);
        this.extraMedewerkerDidChange.bind(this);
        this.opmerkingMbberDidChange.bind(this);
        this.opmerkingVoorRadioloogDidChange.bind(this);
        this.operatieRechtsDidChange.bind(this);
        this.operatieLinksDidChange.bind(this);
        this.aanvullendeInformatieOperatieDidChange.bind(this);
    }

    suboptimaleInsteltechniekDidChange = (value: ?string): void => {
        if (value !== this.props.suboptimaleInsteltechniek) {
            this.props.verwerkInsteltechniek(this.props.afspraakId, value);
        }
    };

    redenFotoBesprekingDidChange = (value: ?string): void => {
        if (value !== this.props.redenFotobespreking) {
            this.props.verwerkRedenFotobespreking(this.props.afspraakId, value);
        }
    };

    extraMedewerkerDidChange = (value: any): void => {
        if (undefinedOrNull(value)) {
            this.props.verwerkExtraMedewerkerId(this.props.afspraakId, null);
        } else if (value[0] !== this.props.extraMedewerkerId) {
            this.props.verwerkExtraMedewerkerId(this.props.afspraakId, value[0]);
        }
    };

    opmerkingMbberDidChange = (value: string): void => {
        if (!zelfdeTekst(value, this.props.opmerkingMbber)) {
            this.props.verwerkOpmerkingMbber(this.props.afspraakId, value);
        }
    };

    opmerkingVoorRadioloogDidChange = (value: string): void => {
        if (!zelfdeTekst(value, this.props.opmerkingVoorRadioloog)) {
            this.props.verwerkOpmerkingVoorRadioloog(this.props.afspraakId, value);
        }
    };

    operatieRechtsDidChange = (): void => {
        this.props.verwerkOperatieRechtsChanged(this.props.afspraakId, !this.props.operatieRechts);
    };

    operatieLinksDidChange = (): void => {
        this.props.verwerkOperatieLinksChanged(this.props.afspraakId, !this.props.operatieLinks);
    };

    aanvullendeInformatieOperatieDidChange = (value: string): void => {
        if (!zelfdeTekst(value, this.props.aanvullendeInformatieOperatie)) {
            this.props.verwerkaanvullendeInformatieOperatie(this.props.afspraakId, value);
        }
    };

    render() {
        return (
            <Card className={'visuele-inspectie-row onderzoek-component'}>
                <CardBody>
                    <h6>Suboptimale insteltechniek</h6>
                    <DropdownValue id={'suboptimaleInsteltechniek'} value={this.props.suboptimaleInsteltechniek}
                                   disabled={this.props.disabled} options={optionInsteltechniek} valueToLabel={getSuboptimaleInsteltechniekBeschrijving}
                                   lavendel={true}
                                   handleChange={this.suboptimaleInsteltechniekDidChange}/>

                    <div className={'mbb-signalering-row'}>
                        <h6>Reden fotobespreking</h6>
                        <DropdownValue id={'redenFotobespreking'} value={this.props.redenFotobespreking}
                                       disabled={this.props.disabled} options={optionFotobespreking} valueToLabel={getRedenFotobesprekingString}
                                       lavendel={true}
                                       handleChange={this.redenFotoBesprekingDidChange}/>
                    </div>

                    <div className={'mbb-signalering-row'}>
                        <h6>Extra MBB'er</h6>
                        <DropdownValue id={'extraMedewerkerId'} value={this.props.extraMedewerkerId === null || this.props.extraMedewerkerId === undefined ?
                            null : ['none', this.props.seGebruikers.get(this.props.extraMedewerkerId)]}
                                       disabled={this.props.disabled}
                                       options={Array.from(this.props.seGebruikers).
                                           filter(this.zonderIngelogdeGebruikerFilterFuncion.bind(this)).
                                           sort((a, b) => String(a[1]) > String(b[1]) ? 1 : -1)}
                                       valueToLabel={(seGebruiker) => (seGebruiker[1] !== undefined ? seGebruiker[1] : '')}
                                       lavendel={true}
                                       handleChange={this.extraMedewerkerDidChange}/>
                    </div>

                    <div className={'mbb-signalering-row'}>
                        <h6>Opmerking voor MBB'er</h6>
                        <TextAreaValue value={this.props.opmerkingMbber || ''} disabled={this.props.disabled}
                                       maxLength={maxOpmerkingLengte}
                                       onChange={this.opmerkingMbberDidChange} color={'lavender'}>
                        </TextAreaValue>
                    </div>

                    <div className={'mbb-signalering-row'}>
                        <h6>Opmerking voor radioloog</h6>
                        <TextAreaValue value={this.props.opmerkingVoorRadioloog || ''} disabled={this.props.disabled}
                                       maxLength={maxOpmerkingLengte}
                                       onChange={this.opmerkingVoorRadioloogDidChange} color={'lavender'}>
                        </TextAreaValue>
                    </div>

                    <div className={'mbb-signalering-row'}>
                        <Row noGutters>
                            <Label check className={'form-check-inline'}>
                                <Input type='checkbox' className='se-value' disabled={this.props.disabled} checked={this.props.operatieRechts}
                                       onChange={this.operatieRechtsDidChange.bind(this)}/>
                                Operatie R
                            </Label>
                            <Label check className={'form-check-inline'}>
                                <Input type='checkbox' className='se-value' disabled={this.props.disabled} checked={this.props.operatieLinks}
                                       onChange={this.operatieLinksDidChange.bind(this)}/>
                                Operatie L
                            </Label>
                        </Row>
                        <TextAreaValue className={'mbb-signalering-text-area-input'} value={this.props.aanvullendeInformatieOperatie || ''} placeholder={'Aanvullende informatie'}
                                       disabled={this.props.disabled}
                                       maxLength={maxOpmerkingLengte}
                                       onChange={this.aanvullendeInformatieOperatieDidChange} color={'lavender'}>
                        </TextAreaValue>
                    </div>
                </CardBody>
            </Card>
        );
    }

    zonderIngelogdeGebruikerFilterFuncion(gebruiker: [string, string]) {

        return gebruiker[0] != this.props.ingelogdeGebruikerID;
    }
}
