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
import {getGeenHuisartsLabel, getHuisartsVolledigAdres} from '../../../datatypes/Huisarts';
import LabelValue from '../../generic/LabelValue';
import {Button, Col, Row} from 'reactstrap';
import PaneelNaam from '../../generic/PaneelNaam';
import Paneel from '../../generic/Paneel';
import type {Form} from '../../../datatypes/Form';
import {Afspraak} from '../../../datatypes/Afspraak';
import HuisartsZoekenView from './HuisartsZoekenView';

type HuisartsgegevensProps = {
    huisarts: Huisarts,
}
type GeenHuisartsProps = {
    geenHuisartsOptie: ?GeenHuisartsOption
}
export type HuisartsViewProps = GeenHuisartsProps
    & {
    afspraak: Afspraak,
    huisarts: ?Huisarts,
    clientGegevensForm: Form,
    isValid: boolean,
    disabled: boolean,
    clientPlaats: string,

    onKiesHuisarts: (afspraak: Afspraak, huisarts: Huisarts) => void;
    onKiesGeenHuisartsOptie: (afspraak: Afspraak, geenHuisartsOptie: GeenHuisartsOption) => void;
}

type HuisartsViewState = {
    toonHuisartsZoekenPopup: boolean
}

export default class HuisartsView extends Component<HuisartsViewProps, HuisartsViewState> {

    constructor(props: HuisartsViewProps) {
        super(props);
        this.props = props;
        this.state = {
            toonHuisartsZoekenPopup: false,
        };
        this.toggleHuisartsZoekenPopup = this.toggleHuisartsZoekenPopup.bind(this);
        this.selecteerHuisarts = this.selecteerHuisarts.bind(this);
        this.selecteerGeenHuisarts = this.selecteerGeenHuisarts.bind(this);
    }

    toggleHuisartsZoekenPopup = () => {
        this.setState({
            toonHuisartsZoekenPopup: !this.state.toonHuisartsZoekenPopup,
        });
    };

    selecteerHuisarts = (huisarts: Huisarts) => {
        this.props.onKiesHuisarts(this.props.afspraak, huisarts);
        this.toggleHuisartsZoekenPopup();
    };

    selecteerGeenHuisarts = (geenHuisartsOptie: GeenHuisartsOption) => {
        this.props.onKiesGeenHuisartsOptie(this.props.afspraak, geenHuisartsOptie);
        this.toggleHuisartsZoekenPopup();
    };

    render() {
        const huisarts: ?Huisarts = this.props.huisarts;
        const geenHuisartsOptie: ?GeenHuisartsOption = this.props.geenHuisartsOptie;
        return <Paneel>
            <PaneelNaam titel={'Huisarts*'}>
                {!this.props.disabled && <Button className={'float-right btn-secondary-se'} onClick={this.toggleHuisartsZoekenPopup}>Zoek huisarts</Button>}
            </PaneelNaam>
            {!this.props.isValid && <div className={'invalid-feedback'} style={{display: 'block'}}>Het selecteren van een huisarts of één van de andere opties is verplicht.</div>}
            {huisarts ? <HuisartsgegevensView huisarts={huisarts}/> : <GeenHuisartsView geenHuisartsOptie={geenHuisartsOptie}/>}
            <HuisartsZoekenView clientPlaats={this.props.clientPlaats} geenHuisartsOptie={geenHuisartsOptie} selecteerHuisarts={this.selecteerHuisarts}
                                selecteerGeenHuisarts={this.selecteerGeenHuisarts}
                                afspraak={this.props.afspraak} huisarts={huisarts} isOpen={this.state.toonHuisartsZoekenPopup}
                                toggle={this.toggleHuisartsZoekenPopup}/>
        </Paneel>;
    }

}

const HuisartsgegevensView = (huisartsProps: HuisartsgegevensProps) => {
    const huisarts: Huisarts = huisartsProps.huisarts;
    return (<Row noGutters>
        <Col md={6}>
            <LabelValue label={'Naam huisarts'} mdLabel={4} mdValue={8} value={huisarts.naamHuisarts}/>
            <LabelValue label={'Weergave naam'} mdLabel={4} mdValue={8} value={huisarts.weergaveNaam}/>
            <LabelValue label={'Praktijknaam'} mdLabel={4} mdValue={8} value={huisarts.praktijknaam}/>
        </Col>
        <Col md={6}>
            <LabelValue label={'Adres'} mdLabel={4} mdValue={8} value={getHuisartsVolledigAdres(huisarts)}/>
        </Col>

    </Row>);
};

const GeenHuisartsView = (huisartsProps: GeenHuisartsProps) => {
    const geenHuisartsOptie: ?GeenHuisartsOption = huisartsProps.geenHuisartsOptie;
    return (<Row>
        <Col md={6}>
            {geenHuisartsOptie && <LabelValue mdLabel={4} mdValue={8} label={'Anders'} value={getGeenHuisartsLabel(huisartsProps.geenHuisartsOptie)}/>}
        </Col>
    </Row>);
};
