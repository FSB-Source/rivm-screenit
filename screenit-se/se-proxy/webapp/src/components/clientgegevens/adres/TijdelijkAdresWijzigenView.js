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
import {Button, Col, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import DatePicker, {registerLocale} from 'react-datepicker';
import moment from 'moment';
import {datumFormaat, vandaagISO} from '../../../util/DateUtil';
import {store} from '../../../Store';
import {createActionSetTijdelijkAdres} from '../../../actions/ClientActions';
import type {TijdelijkAdres} from '../../../datatypes/TijdelijkAdres';
import {getMandatory} from '../../../util/MapUtil';
import nl from 'date-fns/locale/nl';

registerLocale('nl', nl);

export type TijdelijkAdresWijzigenProps = {
    isOpen: boolean,
    toggle: () => void,
    clientId: number,
}

let invoerenIngedrukt = false;

export default class TijdelijkAdresWijzigenView extends Component<TijdelijkAdresWijzigenProps, TijdelijkAdres> {
    constructor(props: TijdelijkAdresWijzigenProps) {
        super(props);
        this.props = props;

        const tijdelijkAdres: TijdelijkAdres = getMandatory(store.getState().clientenById, this.props.clientId).tijdelijkAdres;
        if (tijdelijkAdres) {
            this.state = {
                straat: tijdelijkAdres.straat || '',
                huisnummer: tijdelijkAdres.huisnummer || 0,
                huisletter: tijdelijkAdres.huisletter || '',
                huisnummerToevoeging: tijdelijkAdres.huisnummerToevoeging || '',
                huisnummerAanduiding: tijdelijkAdres.huisnummerAanduiding || '',
                postcode: tijdelijkAdres.postcode || '',
                plaats: tijdelijkAdres.plaats || '',
                startDatum: tijdelijkAdres.startDatum || vandaagISO(),
                eindDatum: tijdelijkAdres.eindDatum || null,
            };
        } else {
            this.state = {
                straat: '',
                huisnummer: 0,
                huisletter: '',
                huisnummerToevoeging: '',
                huisnummerAanduiding: '',
                postcode: '',
                plaats: '',
                startDatum: vandaagISO(),
                eindDatum: null,
            };
        }
    }

    tijdelijkAdresChangeEvent = (event: Event) => {
        const target = event.target;
        const veranderdeWaarde = {};
        if (target instanceof HTMLInputElement || target instanceof HTMLTextAreaElement) {
            if (target.id === 'postcode') {
                veranderdeWaarde[target.id] = target.value.toUpperCase();
            } else if (target.id === 'straat' || target.id === 'plaats') {
                veranderdeWaarde[target.id] = target.value.charAt(0).toUpperCase() + target.value.slice(1);
            } else {
                veranderdeWaarde[target.id] = target.value;
            }
            this.setState(veranderdeWaarde);
        }
    };

    changeVanafDatum = (datum: string) => {
        this.setState({startDatum: moment(datum).format('YYYY-MM-DD')});
    };

    changeTotMetDatum = (datum: string) => {
        this.setState({eindDatum: moment(datum).format('YYYY-MM-DD')});
    };

    clearEindDatum = () => {
        this.setState({eindDatum: null});
    };

    invoeren = () => {
        invoerenIngedrukt = true;
        this.forceUpdate();
        if (this.valideTijdelijkAdres().length <= 0) {
            store.dispatch(createActionSetTijdelijkAdres(this.props.clientId, this.state));
            this.props.toggle();
            invoerenIngedrukt = false;
        }
    };

    valideTijdelijkAdres = (): Array<string> => {
        let afkeurRedenen = [];

        if (!invoerenIngedrukt) {
            return afkeurRedenen;
        }

        this.state.straat.length > 43 ? afkeurRedenen.push('Straatnaam te lang') : null;
        this.state.straat.length <= 0 ? afkeurRedenen.push('Geen straatnaam ingevuld') : null;
        this.state.huisnummer > 99999999 ? afkeurRedenen.push('Huisnummer te groot') : null;
        this.state.huisnummer <= 0 ? afkeurRedenen.push('Geen huisnummer ingevuld') : null;
        if (this.state.huisletter) {
            this.state.huisletter.length > 1 ? afkeurRedenen.push('Huisletter moet precies één letter zijn') : null;
            !/^[a-zA-Z]+$/.test(this.state.huisletter) ? afkeurRedenen.push('Ongeldig huisletter') : null;
        }
        if (this.state.huisnummerToevoeging) {
            this.state.huisnummerToevoeging.length > 26 ? afkeurRedenen.push('Huisnummertoevoeging te lang') : null;
        }
        if (this.state.huisnummerAanduiding) {
            this.state.huisnummerAanduiding.length > 2 ? afkeurRedenen.push('Huisnummeraanduiding mag maximaal 2 karakters lang zijn') : null;
        }
        !/^[1-9][0-9]{3}[\s]?[A-Za-z]{2}$/i.test(this.state.postcode) ? afkeurRedenen.push('Geen of ongeldige postcode ingevuld') : null;
        this.state.plaats.length > 200 ? afkeurRedenen.push('Plaatsnaam is te lang') : null;
        this.state.plaats.length <= 0 ? afkeurRedenen.push('Geen plaatsnaam ingevuld') : null;
        if (!this.state.startDatum) {
            afkeurRedenen.push('Geen startdatum gevuld');
        } else if (this.state.eindDatum) {
            this.state.eindDatum < this.state.startDatum ? afkeurRedenen.push('Einddatum moet na startdatum liggen') : null;
            this.state.eindDatum < vandaagISO() ? afkeurRedenen.push('Einddatum moet na vandaag liggen') : null;
        }

        return afkeurRedenen;
    };

    render() {
        if (!this.props.isOpen) {
            invoerenIngedrukt = false;
        }
        return (<Modal isOpen={this.props.isOpen} toggle={this.props.toggle} size={'lg'}>
            <ModalHeader>Tijdelijk adres wijzigen</ModalHeader>
            <ModalBody>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={10}>
                        <ul>
                            {
                                this.valideTijdelijkAdres().map((foutmelding: string, index: number) => {
                                    return (
                                        <li className={'text-danger'} key={index}>
                                            {foutmelding}
                                        </li>);
                                })
                            }
                        </ul>
                    </Col>
                </div>
            </ModalBody>
            <ModalBody>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Straatnaam*</Label>
                    </Col>
                    <Col md={6}>
                        <Input placeholder={'Straatnaam'} id={'straat'} value={this.state.straat} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                </div>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Huisnummer* en letter</Label>
                    </Col>
                    <Col md={2}>
                        <Input placeholder={'12'} id={'huisnummer'} value={this.state.huisnummer > 0 ? this.state.huisnummer : ''}
                               onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                    <Col md={2}>
                        <Input placeholder={''} id={'huisletter'} value={this.state.huisletter} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                </div>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Huisnummertoevoeging</Label>
                    </Col>
                    <Col md={4}>
                        <Input placeholder={''} id={'huisnummerToevoeging'} value={this.state.huisnummerToevoeging} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                </div>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Aanduiding bij huisnummer</Label>
                    </Col>
                    <Col md={2}>
                        <Input maxLength={2} placeholder={''} id={'huisnummerAanduiding'} value={this.state.huisnummerAanduiding} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                </div>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Postcode* en plaats*</Label>
                    </Col>
                    <Col md={2}>
                        <Input maxLength={6} placeholder={'1234AA'} id={'postcode'} value={this.state.postcode} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                    <Col md={4}>
                        <Input placeholder={'Plaatsnaam'} id={'plaats'} value={this.state.plaats} onChange={this.tijdelijkAdresChangeEvent}/>
                    </Col>
                </div>
                <div className={'form-row mb-2'}>
                    <Col md={1}/>
                    <Col md={4}>
                        <Label>Periode*</Label>
                    </Col>

                    <Col md={6}>
                        <div className={'form-row mb-2'}>

                            <Col md={5} className={'adres-datumkiezer'}>
                                <div className={'form-row mb-2'}>
                                    <Col md={10}>
                                        <DatePicker
                                            value={datumFormaat(this.state.startDatum)}
                                            className="clickable"
                                            locale="nl"
                                            onChange={this.changeVanafDatum}
                                        />
                                    </Col>
                                    <Col md={1}>
                                        <i className="fa fa-calendar px-1 py-1 float-left"/>
                                    </Col>
                                </div>
                            </Col>
                            <p className={'tot-met-paragraaf'}>
                                &nbsp;&nbsp;t/m&nbsp;
                            </p>
                            <Col md={5} className={'adres-datumkiezer'}>
                                <div className={'form-row mb-2'}>
                                    <Col md={1} id={
                                        this.state.eindDatum ? 'clear-adres-datumkiezer' : 'clear-adres-datumkiezer-hidden'
                                    } onClick={this.clearEindDatum}>
                                        <i className="fa fa-trash px-1 py-1 float-left"/>
                                    </Col>
                                    <Col md={10}>
                                        <DatePicker
                                            value={this.state.eindDatum ? datumFormaat(this.state.eindDatum) : ''}
                                            className="clickable"
                                            locale="nl"
                                            onChange={this.changeTotMetDatum}
                                        />
                                    </Col>
                                    <Col md={1}>
                                        <i className="fa fa-calendar px-1 py-1 float-left"/>
                                    </Col>
                                </div>
                            </Col>
                        </div>
                    </Col>
                </div>
            </ModalBody>
            <ModalFooter>

                <div className={'form-row mb-2'}>
                    <Col md={4} className={'offset-8'}>
                        <Button color="primary" onClick={this.invoeren}>
                            Invoeren
                        </Button>
                        &nbsp;
                        <Button color="secondary" onClick={this.props.toggle}>
                            Annuleren
                        </Button>
                    </Col>
                </div>
            </ModalFooter>
        </Modal>);
    }
}
