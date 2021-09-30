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
import DatumkiezerContainer from '../daglijst/DatumkiezerContainer';
import DagproductieContainer from './DagproductieContainer';
import DagStatistiekenContainer from './DagStatistiekenContainer';
import DagSynchronisatieContainer from './DagSynchronisatieContainer';
import DagAfrondstatusContainer from './DagAfrondstatusContainer';
import {Button} from 'reactstrap';
import type {Dagverslag} from '../../datatypes/Dagverslag';

type DagverslagProps = {
    showAfsprakenDoorvoerenPopup: () => {},
    cancelPopup: () => {},
    afsprakenDoorvoerenDisabled: boolean,
    dagverslag: Map<string, Dagverslag>,
    daglijstDatum: string,
    doorvoerenFeedback: string,
}

export default class DagverslagView extends React.Component<DagverslagProps> {

    constructor(props: DagverslagProps) {
        super(props);
        this.props = props;
    }

    render() {
        const clickAfsprakenDoorvoeren = () => this.props.showAfsprakenDoorvoerenPopup();
        return (
            <div className='dagverslag-lijst'>
                <div className='row row-dagverslag-top'>
                    <div className='col-3'>
                        <Button className={(this.props.afsprakenDoorvoerenDisabled) ? 'btn-primary-se disabled' : 'btn-primary-se'}
                                onClick={clickAfsprakenDoorvoeren}>
                            Dag afsluiten
                        </Button>
                    </div>
                    <div className='col-9'>
                        <div className='daglijst-blokken-rechts'>
                            <div className='dagverslag-datumkiezer'>
                                <DatumkiezerContainer/>
                            </div>
                        </div>
                    </div>
                </div>
                <DagproductieContainer/>
                <div className='row row-no-gutters'>
                    <div className='col-6 col-no-gutters-left'>
                        <DagStatistiekenContainer/>
                    </div>
                    <div className='col-6 col-no-gutters-right'>
                        <DagSynchronisatieContainer/><DagAfrondstatusContainer/>
                    </div>
                </div>
            </div>
        );
    }
}
