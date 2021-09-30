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

import {connect} from 'react-redux';

import type {Afspraak} from '../../datatypes/Afspraak';
import type {AfspraakRijProps} from './AfspraakRijView';
import AfspraakRijView from './AfspraakRijView';
import type {State} from '../../datatypes/State';
import {vandaagISO} from '../../util/DateUtil';
import {getIfExists} from '../../util/MapUtil';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import {navigeerNaarClientAfspraak} from './AfspraakOverzichtView';

const mapStateToProps = (state: State, ownProps: AfspraakRijProps) => {

    const onderzoek: Onderzoek | null = getIfExists(state.onderzoekByAfspraakId, ownProps.afspraak.id);

    return {
        onderzoekStatus: onderzoek ? onderzoek.status : null,
        klikbaar: ownProps.afspraak ? ownProps.afspraak.vanafDatum === vandaagISO() || ownProps.afspraak.status !== 'VERWACHT' : false,
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        onRijKlik(afspraak: Afspraak) {
            navigeerNaarClientAfspraak(afspraak);
        },
    };
};

const AfspraakRijContainer = connect(mapStateToProps, mapDispatchToProps)(AfspraakRijView);

export default AfspraakRijContainer;
