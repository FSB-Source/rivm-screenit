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
import {Card, CardBody} from 'reactstrap';
import DropdownValue from '../../generic/DropdownValue';
import LabelInput from '../../generic/LabelInput';
import type {OnvolledigOnderzoekOption} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek';
import {allOnvolledigOnderzoekOptions, getOnvolledigOnderzoekLabel} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek';
import type {OnderbrokenOnderzoekOption} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek';
import {allOnderbrokenOnderzoekOptions, getOnderbrokenOnderzoekLabel} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek';
import type {ExtraFotosReden} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/ExtraFotosReden';
import {allExtraFotosRedenOptions, getExtraFotosRedenLabel} from '../../../datatypes/visueleinspectie/aanvullendeinformatie/ExtraFotosReden';
import CheckboxValue from '../../generic/CheckboxValue';
import MultiselectValue from '../../generic/MultiselectValue';
import type {Form, FORM_FIELD_ID} from '../../../datatypes/Form';
import ValidationInputContainer from '../../generic/ValidationInputContainer';
import type {Zorginstelling} from '../../../datatypes/Zorginstelling';
import TextAreaValue from '../../generic/TextAreaValue';
import AlleenOnlineButton from '../../generic/AlleenOnlineButton';

export type AanvullendeInformatieProps = {
    online: boolean,
    afspraakId: number,
    clientId: number,
    jaren: Array<number>,
    zorginstellingen: Array<Zorginstelling>,
    eerderMammogramZorginstelling: ?Zorginstelling,
    eerderMammogramJaartal: ?number,
    onvolledigOnderzoek: ?OnvolledigOnderzoekOption,
    onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption,
    extraFotosRedenen: ?Array<ExtraFotosReden>,
    dubbeleTijd: boolean,
    dubbeleTijdReden: ?string,
    adviesHuisarts: ?string,
    onderzoekForm: Form,
    dubbeleTijdDisabled: boolean,

    disabled: boolean,

    verwerkEerderMammogramZorginstelling: (afspraakId: number, zorginstelling: ?Zorginstelling) => void,
    verwerkEerderMammogramJaartal: (afspraakId: number, jaartal: ?number) => void,
    verwerkOnvolledigOnderzoek: (afspraakId: number, onvolledigOnderzoek: ?OnvolledigOnderzoekOption) => void,
    verwerkOnderbrokenOnderzoek: (afspraakId: number, onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption) => void,
    verwerkExtraFotosReden: (afspraakId: number, extraFotosReden: Array<ExtraFotosReden>) => void,
    verwerkDubbeleTijd: (afspraakId: number, clientId: number, dubbeleTijd: boolean, dubbeleTijdReden: ?string, form: Form) => void,
    verwerkDubbeleTijdReden: (afspraakId: number, clientId: number, dubbeleTijdReden: string) => void,
    verwerkAdviesHuisarts: (afspraakId: number, adviesHuisarts: string) => void,
    adhocKwaliteitscontrolePopup: (onderzoekId: number) => void,

    updateField: <T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: ?boolean) => void;
}

export const DUBBELE_TIJD_FIELD_ID: FORM_FIELD_ID = {formId: 'onderzoek', fieldId: 'dubbeleTijdReden'};

export default class AanvullendeInformatieView extends Component<AanvullendeInformatieProps> {

    constructor(props: AanvullendeInformatieProps) {
        super(props);
        this.props = props;
        this.zorginstellingDidChange.bind(this);
        this.jaartalDidChange.bind(this);
        this.onvolledigOnderzoekDidChange.bind(this);
        this.onderbrokenOnderzoekDidChange.bind(this);
        this.extraFotosRedenDidChange.bind(this);
        this.dubbeleTijdDidChange.bind(this);
        this.dubbeleTijdRedenDidChange.bind(this);
        this.adviesHuisartsDidChange.bind(this);
    }

    zorginstellingDidChange = (value: ?Zorginstelling): void => {
        if (value !== this.props.eerderMammogramZorginstelling) {
            this.props.verwerkEerderMammogramZorginstelling(this.props.afspraakId, value);
        }
    };

    jaartalDidChange = (value: ?number): void => {
        if (value !== this.props.eerderMammogramJaartal) {
            this.props.verwerkEerderMammogramJaartal(this.props.afspraakId, value);
        }
    };

    onvolledigOnderzoekDidChange = (value: ?OnvolledigOnderzoekOption): void => {
        this.props.verwerkOnvolledigOnderzoek(this.props.afspraakId, value);
    };
    onderbrokenOnderzoekDidChange = (value: ?OnderbrokenOnderzoekOption): void => {
        this.props.verwerkOnderbrokenOnderzoek(this.props.afspraakId, value);
    };
    extraFotosRedenDidChange = (selectedOptions: Array<ExtraFotosReden>): void => {
        this.props.verwerkExtraFotosReden(this.props.afspraakId, selectedOptions);
    };
    dubbeleTijdDidChange = (value: boolean): void => {
        this.props.verwerkDubbeleTijd(this.props.afspraakId, this.props.clientId, value, this.props.dubbeleTijdReden, this.props.onderzoekForm);
    };
    dubbeleTijdRedenDidChange = (value: string): void => {
        this.props.verwerkDubbeleTijdReden(this.props.afspraakId, this.props.clientId, value);
    };
    adviesHuisartsDidChange = (value: string): void => {
        if (value !== this.props.adviesHuisarts) {
            if (value === '' && this.props.adviesHuisarts === null) {
                return;
            }
            this.props.verwerkAdviesHuisarts(this.props.afspraakId, value);
        }
    };

    render() {
        return (
            <Card className={'visuele-inspectie-row onderzoek-component'}>
                <CardBody>
                    <h6>Zorginstelling eerdere mammogram</h6>
                    <DropdownValue id={'eerderMammogramZorginstellingId'} value={this.props.eerderMammogramZorginstelling}
                                   disabled={this.props.disabled}
                                   options={this.props.zorginstellingen.sort((a, b) => a.naam > b.naam ? 1 : -1)}
                                   valueToLabel={(zi: Zorginstelling) => (zi.naam)}
                                   lavendel={true}
                                   handleChange={this.zorginstellingDidChange}/>
                    <br/>
                    <LabelInput label={'Jaartal eerdere mammogram'} input={
                        <DropdownValue id={'eerderMammogramJaartal'} value={this.props.eerderMammogramJaartal}
                                       disabled={this.props.disabled}
                                       options={this.props.jaren}
                                       lavendel={true}
                                       handleChange={this.jaartalDidChange}/>}
                    />
                    <LabelInput label={'Onvolledig onderzoek'} input={
                        <DropdownValue id={'onvolledigOnderzoek'} disabled={this.props.disabled}
                                       value={this.props.onvolledigOnderzoek}
                                       options={allOnvolledigOnderzoekOptions} valueToLabel={getOnvolledigOnderzoekLabel}
                                       lavendel={true} handleChange={this.onvolledigOnderzoekDidChange}/>}
                    />
                    <LabelInput label={'Onderbroken onderzoek'} input={
                        <DropdownValue id={'onderbrokenOnderzoek'} disabled={this.props.disabled}
                                       value={this.props.onderbrokenOnderzoek}
                                       options={allOnderbrokenOnderzoekOptions} valueToLabel={getOnderbrokenOnderzoekLabel}
                                       lavendel={true} handleChange={this.onderbrokenOnderzoekDidChange}/>}
                    />
                    <LabelInput label={'Extra Foto\'s'} input={
                        <MultiselectValue id={'extraFotos'} multiple disabled={this.props.disabled || this.props.onvolledigOnderzoek === 'ZONDER_FOTOS'}
                                          value={this.props.extraFotosRedenen ? this.props.extraFotosRedenen.map((val) => {
                                              return {value: val, label: getExtraFotosRedenLabel(val)};
                                          }) : []}
                                          options={allExtraFotosRedenOptions.map((v) => {
                                              return {value: v, label: getExtraFotosRedenLabel(v)};
                                          })}
                                          handleChange={this.extraFotosRedenDidChange}/>}
                    />
                    <div className={'mbb-signalering-row'}>
                        <CheckboxValue label={'Dubbele tijd'} checked={this.props.dubbeleTijd} disabled={this.props.disabled || this.props.dubbeleTijdDisabled}
                                       handleChange={this.dubbeleTijdDidChange}/>
                        {this.props.dubbeleTijd ? <ValidationInputContainer fieldId={DUBBELE_TIJD_FIELD_ID} type={'textarea'} value={this.props.dubbeleTijdReden || ''}
                                                                            transformValue={(reden: string) => {
                                                                                return {dubbeleTijd: this.props.dubbeleTijd, dubbeleTijdReden: reden};
                                                                            }}
                                                                            disabled={this.props.disabled}
                                                                            maxLength={255}
                                                                            showRedErrorBackground={true}
                                                                            onChange={this.dubbeleTijdRedenDidChange} placeholder='' color={'lavender'} required/> : null}
                    </div>
                    <div className={'mbb-signalering-row'}>
                        <h6>Advies huisarts</h6>
                        <TextAreaValue className={'mbb-signalering-text-area-input'} value={this.props.adviesHuisarts || ''}
                                       disabled={this.props.disabled}
                                       maxLength={255} placeholder=''
                                       onChange={this.adviesHuisartsDidChange} color={'lavender'}>
                        </TextAreaValue>
                    </div>
                    <div className={'mbb-signalering-row'}>
                        <AlleenOnlineButton label={'Meekijkverzoek LRCB'} online={this.props.online} onClick={() => {
                            this.props.adhocKwaliteitscontrolePopup(this.props.afspraakId);
                        }}/>
                    </div>
                </CardBody>
            </Card>
        );
    }
}
