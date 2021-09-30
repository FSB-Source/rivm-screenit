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
import {Mammograaf} from '../../datatypes/Mammograaf';
import MammograafknopView from './MammograafknopView';
import WerkstationZonderMammograafknopView from './WerkstationZonderMammograafknopView';

export type WerkstationkiezerProps = {
    mammografen: Array<Mammograaf>;
}

type WerkstationkiezerState = {}

export default class WerkstationkiezerView extends Component<WerkstationkiezerProps, WerkstationkiezerState> {

    constructor(props: WerkstationkiezerProps) {
        super(props);
        this.props = props;
        this.state = {};
    }

    render() {
        return (
            <div className={'text-center'} style={{marginTop: 130}}>
                <div className={'align-middle'}>Klik op het werkstation waarvan je de werking gaat testen:</div>
                {this.props.mammografen.map((mammograaf: Mammograaf) =>
                    <div key={mammograaf.id} style={{marginTop: 10}}><MammograafknopView mammograaf={mammograaf}/></div>)}
                <div style={{marginTop: 10}}><WerkstationZonderMammograafknopView/></div>
            </div>
        );
    }
}
