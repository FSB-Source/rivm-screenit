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

type AmputatieWaarschuwingProps = {}

export default class AmputatieWaarschuwingPopup extends React.Component<AmputatieWaarschuwingProps> {

    constructor(props: AmputatieWaarschuwingProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <div>
                <p>Er is een verschil geconstateerd tussen de gemaakte beelden en het amputatiekruis. Controleer of het amputatiekruis op de juiste plek staat en controleer de
                    ingestelde procedure op de mammograaf.</p>
            </div>
        );
    }
};
