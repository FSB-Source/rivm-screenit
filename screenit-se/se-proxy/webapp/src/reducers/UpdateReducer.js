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

import type {PendingUpdates} from '../datatypes/PendingUpdates';
import type {UpdateActions} from '../actions/UpdateAction';
import {QUEUE_DAGLIJST_VERVERSEN} from '../actions/UpdateAction';
import {CLEAR_AFSPRAKEN} from '../actions/AfspraakActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';
import {vandaagISO} from '../util/DateUtil';

const pendingUpdatesReducer = (stateSlice: PendingUpdates | null = null, action: UpdateActions): ?PendingUpdates => {
    switch (action.type) {
        case QUEUE_DAGLIJST_VERVERSEN:
            return {
                moetDaglijstNogVerversen: true,
            };
        case CLEAR_AFSPRAKEN:
            return action.datum !== vandaagISO() ? stateSlice : {
                moetDaglijstNogVerversen: false,
            };
        case CLEAR_CACHE:
            return null;
        default:
            return stateSlice;
    }
};

export default pendingUpdatesReducer;
