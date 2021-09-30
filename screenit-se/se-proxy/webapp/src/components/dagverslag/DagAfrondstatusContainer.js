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
import DagAfrondstatusView from './DagAfrondstatusView';
import type {State} from '../../datatypes/State';
import {Afspraak} from '../../datatypes/Afspraak';
import {getDagAfspraken} from '../../util/AfsprakenUtil';

const mapStateToProps = (state: State) => {
    const huidigeDagAfspraken: Array<Afspraak> = getDagAfspraken(state.daglijstDatum);
    return {
        dagverslag: state.dagverslag,
        daglijstDatum: state.daglijstDatum,
        totaalAfgerond: getTotaalAfgerond(huidigeDagAfspraken),
    };
};

const mapDispatchToProps = dispatch => {
    return {};
};

const getTotaalAfgerond = (afspraken: Array<Afspraak>): number => {
    let totalResult = 0;
    afspraken.forEach(function(afspraak) {
        if (afspraak.status === 'BEEINDIGD') {
            totalResult += 1;
        }
    });
    return totalResult;
};

const DagAfrondstatusContainer = connect(mapStateToProps, mapDispatchToProps)(DagAfrondstatusView);

export default DagAfrondstatusContainer;
