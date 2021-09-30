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
import AutorisatieButton from '../generic/AutorisatieButton';
import {Col, Row} from 'reactstrap';
import PaneelNaam from '../generic/PaneelNaam';
import Paneel from '../generic/Paneel';
import {store} from '../../Store';
import {BEEINDIG_KWALITEITSOPNAME, START_KWALITEITSOPNAME} from '../../actions/KwaliteitsopnameOrmActions';
import {getActieveKwaliteitsopname} from '../../restclient/WerklijstRestclient';
import {createStartBezigMetKwaliteitsopnameAction} from '../../actions/BezigMetKwaliteitsopnameActions';
import {parseKwaliteitsopnameVolgNr} from '../../util/KwaliteitsopnameUtil';

export type KwaliteitsopnameProps = {
    onKwaliteitsopnameAction: (actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie) => void,
}

export type KwaliteitsopnameReden =
    'Periodieke kalibratie'
    | 'Regulier Onderhoud'
    | 'Verplaatsing'
    | 'Storing'
    | 'Reparatie'
    | 'Vervanging rontgenbuis' 
    | 'Vervanging detector (LRCB)'
    | 'Foutieve opname in set'
    | 'Zichtbare Beeldverstoring'
    | 'Error tijdens kalibratie'
    | 'Graag SE bellen'
    | 'Op verzoek LRCB'
    | 'Kalibratie op verzoek LRCB'
    | 'Nieuw mammografiesysteem (LRCB)';

const kwaliteitsopnameRedenen: Array<KwaliteitsopnameReden> = [
    'Periodieke kalibratie',
    'Regulier Onderhoud',
    'Verplaatsing',
    'Storing',
    'Reparatie',
    'Vervanging rontgenbuis',
    'Vervanging detector (LRCB)',
    'Foutieve opname in set',
    'Zichtbare Beeldverstoring',
    'Error tijdens kalibratie',
    'Graag SE bellen',
    'Op verzoek LRCB',
    'Kalibratie op verzoek LRCB',
    'Nieuw mammografiesysteem (LRCB)'];

export type VoorOfNaKalibratie = 'Voor kalibratie' | 'Na kalibratie' | 'Geen kalibratie';

const voorOfNaKalibraties: Array<VoorOfNaKalibratie> = [
    'Voor kalibratie',
    'Na kalibratie',
    'Geen kalibratie'];

export type KwaliteitsopnameState = {
    reden: ?KwaliteitsopnameReden;
    voorOfNaKalibratie: ?VoorOfNaKalibratie;
    bezigMetKwaliteitsopname: boolean;
}

export default class KwaliteitsopnameView extends Component<KwaliteitsopnameProps, KwaliteitsopnameState> {

    constructor(props: KwaliteitsopnameProps) {
        super(props);
        this.props = props;
        this.state = {
            reden: null,
            voorOfNaKalibratie: null,
            bezigMetKwaliteitsopname: false,
        };
    }

    componentDidMount(): * {
        const self = this;
        getActieveKwaliteitsopname().then((kwaliteitsopname) => {
            self.setState({
                reden: kwaliteitsopname.reden,
                voorOfNaKalibratie: kwaliteitsopname.voorOfNaKalibratie,
                bezigMetKwaliteitsopname: kwaliteitsopname.accessionNumber != null,
            });
            if (kwaliteitsopname.accessionNumber != null) {
                store.dispatch(createStartBezigMetKwaliteitsopnameAction(parseKwaliteitsopnameVolgNr(kwaliteitsopname.accessionNumber)));
            }
        });
    }

    setReden(reden: KwaliteitsopnameReden) {
        this.setState({...this.state, ...{reden}});
    }

    setVoorOfNaKalibratie(voorOfNaKalibratie: VoorOfNaKalibratie) {
        this.setState({...this.state, ...{voorOfNaKalibratie}});
    }

    render() {
        const state: KwaliteitsopnameState = this.state;

        const startEnabled = state.reden != null && state.voorOfNaKalibratie != null && !state.bezigMetKwaliteitsopname;
        const stopEnabled = state.bezigMetKwaliteitsopname;
        return <div className="onderzoek-scherm">
            <div className="tabpagina">
                <Row>
                    <div className="onderzoek-heading">
                        <h1 className="float-left">Kwaliteitsopname</h1>
                    </div>
                </Row>
                <Row>
                    <Col md={4}>
                        <Paneel id="redenPaneel" className="onderzoek-component">
                            <PaneelNaam titel={'Reden'}/>
                            {
                                kwaliteitsopnameRedenen.map((reden) => {
                                    let actief = (reden === this.state.reden);
                                    return <Row key={reden}>
                                        <label className={'btn'}><input type={'radio'} checked={actief} name={'reden'} disabled={stopEnabled} onClick={() => {
                                            this.setReden(reden);
                                        }}/> {reden}</label>
                                    </Row>;
                                })
                            }
                        </Paneel>
                    </Col>
                    <Col md={4}>
                        <Paneel id="kalibratiePaneel" className="onderzoek-component">
                            <PaneelNaam titel={'Kalibratie'}/>
                            {
                                voorOfNaKalibraties.map((voorOfNa) => {
                                    let actief = (voorOfNa === this.state.voorOfNaKalibratie);
                                    return <Row key={voorOfNa}>
                                        <label className={'btn'}><input type={'radio'} checked={actief} name={'voorOfNaKalibratie'} disabled={stopEnabled} onClick={() => {
                                            this.setVoorOfNaKalibratie(voorOfNa);
                                        }}/> {voorOfNa}</label>
                                    </Row>;
                                })
                            }
                        </Paneel>
                    </Col>
                    <Col md={4}>
                        <Paneel id="werklijstPaneel" className="onderzoek-component">
                            <PaneelNaam titel={'Werklijst mammograaf'}/>
                            <Row>
                                <AutorisatieButton
                                    id={'startButton'}
                                    label={'Start kwaliteitsopname'}
                                    heeftRecht={startEnabled}
                                    rechtNaam={'kwaliteitsopname'}
                                    popovertekst={
                                        state.bezigMetKwaliteitsopname ? 'Beëindig eerst de lopende kwaliteitsopname'
                                            : !state.reden ? 'Kies eerst een reden'
                                            : !state.voorOfNaKalibratie ? 'Kies eerst voor/na/geen kalibratie'
                                                : null}
                                    onClick={() => {
                                        if (startEnabled) {
                                            const reden: KwaliteitsopnameReden = state.reden ? state.reden : 'Regulier Onderhoud';
                                            const voorOfNaKalibratie: VoorOfNaKalibratie = state.voorOfNaKalibratie ? state.voorOfNaKalibratie : 'Voor kalibratie';
                                            this.setState({...state, ...{bezigMetKwaliteitsopname: true}});
                                            this.props.onKwaliteitsopnameAction(START_KWALITEITSOPNAME, reden, voorOfNaKalibratie);
                                        }
                                    }}/>
                            </Row>
                            <Row>
                                <label></label>
                            </Row>
                            <Row>
                                <AutorisatieButton
                                    id={'stopButton'}
                                    label={'Beëindig kwaliteitsopname'}
                                    heeftRecht={stopEnabled}
                                    rechtNaam={'kwaliteitsopname'}
                                    popovertekst={
                                        !stopEnabled ? 'U kunt een kwaliteitsopname pas beëindigen nadat hij gestart is'
                                            : null}
                                    onClick={() => {
                                        if (stopEnabled) {
                                            const reden: KwaliteitsopnameReden = state.reden ? state.reden : 'Regulier Onderhoud';
                                            const voorOfNaKalibratie: VoorOfNaKalibratie = state.voorOfNaKalibratie ? state.voorOfNaKalibratie : 'Voor kalibratie';
                                            this.setState({...state, ...{bezigMetKwaliteitsopname: false}});
                                            this.props.onKwaliteitsopnameAction(BEEINDIG_KWALITEITSOPNAME, reden, voorOfNaKalibratie);
                                        }
                                    }}/>
                            </Row>
                        </Paneel>
                    </Col>;
                </Row>
            </div>
        </div>
            ;
    }
}
