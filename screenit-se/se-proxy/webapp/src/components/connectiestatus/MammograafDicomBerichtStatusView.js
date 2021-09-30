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
import type {ConnectieStatusLevel} from '../../datatypes/connectiestatus/ConnectieStatus';
import {getTijdGeledenTekst} from '../../util/DateUtil';

export type MammograafDicomBerichtStatusProps = {
    messageType: MammograafDicomMessageType,
    level: ConnectieStatusLevel,
    timestamp: string;
    onClick: () => void;
}

export default class MammograafDicomBerichtStatusView extends Component<MammograafDicomBerichtStatusProps> {

    constructor(props) {
        super(props);
        this.props = props;
    }

    render() {
        switch (this.props.level) {
            case 'FAULT':
                return <p><span>Er zijn <u onClick={this.props.onClick}><b>fouten</b></u> opgetreden </span>
                    {this.props.timestamp ? <span>na het laatst succesvolle {this.props.messageType} bericht van: <b>{getTijdGeledenTekst(this.props.timestamp)}</b></span> :
                        <span>bij het eerste {this.props.messageType} bericht sinds het opstarten</span>}</p>;
            case 'OK':
                return <p>Laatst ontvangen succesvolle {this.props.messageType} bericht: <b>{getTijdGeledenTekst(this.props.timestamp)}</b></p>;
            case 'WARN':
                return <p>Er is nog geen succesvol {this.props.messageType} bericht ontvangen</p>;

        }
    }

}
