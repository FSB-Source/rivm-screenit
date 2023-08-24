/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {BezwaarMoment, geenBezwaarMoment} from '../datatypes/Bezwaar';
import {BEZWAAR_TOGGLE, BezwaarActions, RESET_LAATSTE_BEZWAAR_MOMENT} from '../actions/BezwaarReduxAction';

function BezwaarReducer(stateSlice: BezwaarMoment = geenBezwaarMoment, action: BezwaarActions): BezwaarMoment {
    switch (action.type) {
        case RESET_LAATSTE_BEZWAAR_MOMENT:
            return action.bezwaarMoment;
        case BEZWAAR_TOGGLE:
            return stateSlice.map(b => {
                    if (b.type !== action.bezwaarType) {
                        return b
                    }
                    return {
                        ...b,
                        active: !b.active
                    }
                })
        default:
            return stateSlice;
    }
}

export default BezwaarReducer;
