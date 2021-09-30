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
import type {GeenHuisartsOption, Huisarts} from '../../../datatypes/Huisarts';
import {allGeenHuisartsOptions, getGeenHuisartsLabel} from '../../../datatypes/Huisarts';
import {Afspraak} from '../../../datatypes/Afspraak';
import {Button, Col, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import HuisartsLijstContainer from './HuisartsLijstContainer';
import DropdownValue from '../../generic/DropdownValue';

export type HuisartsZoekenProps = {
    afspraak: Afspraak,
    huisarts: ?Huisarts,
    geenHuisartsOptie: ?GeenHuisartsOption,
    isOpen: boolean,
    toggle: () => void,
    clientPlaats: string,

    selecteerHuisarts: (huisarts: Huisarts) => void;
    selecteerGeenHuisarts: (geenHuisartsOptie: GeenHuisartsOption) => void;
}

export type HuisartsZoekFilter = {
    naamHuisarts: string;
    straatnaam: string;
    postcode: string;
    plaats: string;
}

type HuisartsZoekFilterState = {
    fields: HuisartsZoekFilter;
    zoekFilter: HuisartsZoekFilter;
    toonResultaten: boolean;
    moetVerversen: boolean,
}

export default class HuisartsZoekenView extends Component<HuisartsZoekenProps, HuisartsZoekFilterState> {

    constructor(props: HuisartsZoekenProps) {
        super(props);
        this.props = props;
        const defaultFilter = {
            naamHuisarts: '',
            straatnaam: '',
            postcode: '',
            plaats: this.props.clientPlaats ? this.props.clientPlaats : '',
        };
        this.state = {
            fields: {...defaultFilter},
            zoekFilter: defaultFilter,
            toonResultaten: false,
            moetVerversen: false,
        };
        this.kiesGeenHuisarts = this.kiesGeenHuisarts.bind(this);
        this.onNaamHuisartsDidChange = this.onNaamHuisartsDidChange.bind(this);
        this.zoekHuisartsen = this.zoekHuisartsen.bind(this);
        this.vergrendelZoekState = this.vergrendelZoekState.bind(this);
    }

    kiesGeenHuisarts = (geenHuisartsOptie: ?GeenHuisartsOption) => {
        const geenHuisarts: GeenHuisartsOption = (geenHuisartsOptie: any);
        this.props.selecteerGeenHuisarts(geenHuisarts);
    };

    onNaamHuisartsDidChange = (event: Event) => {
        let target = event.target;
        if (target instanceof HTMLInputElement) {
            this.setState({
                ...this.state, fields: {...this.state.fields, naamHuisarts: target.value},
            });
        }
    };

    onStraatnaamDidChange = (event: Event) => {
        let target = event.target;
        if (target instanceof HTMLInputElement) {
            this.setState({
                ...this.state, fields: {...this.state.fields, straatnaam: target.value},
            });
        }
    };

    onPostcodeDidChange = (event: Event) => {
        let target = event.target;
        if (target instanceof HTMLInputElement) {
            this.setState({
                ...this.state, fields: {...this.state.fields, postcode: target.value},
            });
        }
    };

    onPlaatsDidChange = (event: Event) => {
        let target = event.target;
        if (target instanceof HTMLInputElement) {
            this.setState({
                ...this.state, fields: {...this.state.fields, plaats: target.value},
            });
        }
    };

    zoekHuisartsen = () => {
        this.setState({
            ...this.state,
            toonResultaten: true,
            zoekFilter: {
                ...this.state.fields,
            },
            moetVerversen: true,
        });
    };

    vergrendelZoekState = () => {
        this.setState({
            moetVerversen: false,
        });
    };

    handleKeyPress = (event: KeyboardEvent) => {
        if (event.key === 'Enter') {
            this.zoekHuisartsen();
        }
    };

    render() {
        return (<Modal isOpen={this.props.isOpen} toggle={this.props.toggle} size={'lg'}>
            <ModalHeader>Huisarts selecteren</ModalHeader>
            <ModalBody>
                <div className={'form-row mb-2'}>
                    <Col md={6}>
                        <Label>Naam huisarts/huisartsenpraktijk</Label>
                        <Input value={this.state.fields.naamHuisarts} onChange={this.onNaamHuisartsDidChange} onKeyPress={this.handleKeyPress}/>
                    </Col>
                    <Col md={6}>
                        <Label>Straatnaam</Label><Input value={this.state.fields.straatnaam} onChange={this.onStraatnaamDidChange} onKeyPress={this.handleKeyPress}/>
                    </Col>
                </div>
                <div className={'form-row mb-4'}>
                    <Col md={2}>
                        <Label>Postcode</Label><Input maxLength={6} value={this.state.fields.postcode} onChange={this.onPostcodeDidChange} onKeyPress={this.handleKeyPress}/>
                    </Col>
                    <Col md={4}>
                        <Label>Plaats</Label><Input value={this.state.fields.plaats} onChange={this.onPlaatsDidChange} onKeyPress={this.handleKeyPress}/>
                    </Col>
                    <Col md={6}>
                        <Label>&nbsp;</Label>
                        <Button className="btn-primary-se float-right" onClick={this.zoekHuisartsen}>Zoeken</Button>
                    </Col>
                </div>
                {this.state.toonResultaten && <HuisartsLijstContainer huisartsFilter={this.state.zoekFilter} afspraak={this.props.afspraak} toggle={this.props.toggle}
                                                                      onKiesHuisarts={this.props.selecteerHuisarts} moetVerversen={this.state.moetVerversen}
                                                                      vergrendelZoekState={this.vergrendelZoekState}/>}
            </ModalBody>
            <ModalFooter>
                <div className={'form-row'}>
                    <Col md={2} className={'py-1'}>Anders:</Col>
                    <Col md={5} className={this.state.toonResultaten ? 'select-up' : ''}>
                        <DropdownValue id={'geenHuisartsOptie'} value={this.props.geenHuisartsOptie}
                                       disabled={false} required={true} options={allGeenHuisartsOptions} valueToLabel={getGeenHuisartsLabel}
                                       handleChange={this.kiesGeenHuisarts} isWhite={true}/>
                    </Col>
                    <Col md={5}>
                        <Button className={'btn-secondary-se float-right'} onClick={this.props.toggle}>Annuleren</Button>
                    </Col>
                </div>
            </ModalFooter>
        </Modal>);
    }

}
