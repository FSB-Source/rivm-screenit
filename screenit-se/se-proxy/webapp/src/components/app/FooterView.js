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
import type {EnvironmentInfo} from '../../datatypes/EnvironmentInfo';

type FooterProps = {
    environmentInfo: EnvironmentInfo
}

export default class FooterView extends React.Component<FooterProps> {

    constructor(props: FooterProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <footer className={'footer'}>
                <div className={'footer-right'}>
                    <div
                        className={'footer-versie'}>{this.props.environmentInfo ?
                        ('Versie: ' + this.props.environmentInfo.version + '  Build datum/tijd: ' + this.props.environmentInfo.timestamp) : ''} Applicatie-naam:
                        ScreenIT-SE
                    </div>
                    <div
                        className={'footer-omgeving'}>{'Omgeving: ' + this.props.environmentInfo.environment}
                        {this.props.environmentInfo.nfcEnabled ? '' : ', verplichte NFC authenticatie staat uit.'}
                    </div>
                </div>
            </footer>
        );
    }
}
