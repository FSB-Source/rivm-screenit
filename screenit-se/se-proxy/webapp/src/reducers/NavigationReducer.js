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

import {
    CLEAR_NAVIGATION,
    NAVIGATE_TO_CLIENTGEGEVENS,
    NAVIGATE_TO_DAGLIJST,
    NAVIGATE_TO_DAGVERSLAG, NAVIGATE_TO_CONNECTIESTATUS,
    NAVIGATE_TO_KWALITEITSOPNAME,
    NAVIGATE_TO_ONDERZOEK,
    NAVIGATION_RESTORE,
} from '../actions/NavigationActions';

import type {NavigationState} from '../datatypes/Navigation';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const daglijstNavigationState: NavigationState = {
    tab: 'Daglijst',
    subPagina: null,
    clientId: null,
    afspraakId: null,
};

const geenNavigationState: NavigationState = {
    tab: 'Geen',
    subPagina: null,
    clientId: null,
    afspraakId: null,
};

const navigationReducer = (
    stateSlice: NavigationState = geenNavigationState, action: any): NavigationState => {
    switch (action.type === NAVIGATION_RESTORE ? action.targetActionType : action.type) {
        case NAVIGATE_TO_DAGLIJST:
            return daglijstNavigationState;
        case NAVIGATE_TO_CLIENTGEGEVENS:
            return {
                tab: 'Cliëntgegevens',
                subPagina: null,
                clientId: action.clientId,
                afspraakId: action.afspraakId,
            };
        case NAVIGATE_TO_ONDERZOEK:
            return {
                tab: 'Onderzoek',
                subPagina: action.subPagina,
                clientId: action.clientId,
                afspraakId: action.afspraakId,
            };
        case NAVIGATE_TO_DAGVERSLAG:
            return {
                tab: 'Dagverslag',
                subPagina: null,
                clientId: null,
                afspraakId: null,
            };
        case NAVIGATE_TO_KWALITEITSOPNAME:
            return {
                tab: 'Kwaliteitsopname',
                subPagina: null,
                clientId: null,
                afspraakId: null,
            };
        case NAVIGATE_TO_CONNECTIESTATUS:
            return {
                tab: 'Connectiestatus',
                subPagina: null,
                clientId: null,
                afspraakId: null,
            };
        case CLEAR_NAVIGATION:
            return geenNavigationState;
        case CLEAR_CACHE:
            return geenNavigationState;
        default:
            return stateSlice;
    }
};

export default navigationReducer;
