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

import {fetchApiPromise} from '../util/ApiUtil';
import {store} from '../Store';
import {createActionSetDagverslag, createActionSetNietAfgeslotenVanaf} from '../actions/DagverslagActions';
import type {Dagverslag} from '../datatypes/Dagverslag';
import {persistentErrorToast} from '../util/ToastUtil';
import {datumFormaat} from '../util/DateUtil';
import {vulAfspraken} from './DaglijstRestclient';

export const getDagverslag = (daglijstDatum: string): Promise<any> => {
    return fetchApiPromise('GET', 'dagverslag/' + daglijstDatum).then(
        (result) => {
            return result.json().then((dagverslag: Dagverslag) => {
                const nietAfgeslotenVanaf: ?string = dagverslag.nietAfgeslotenVanaf;
                store.dispatch(createActionSetDagverslag(daglijstDatum, dagverslag));
                store.dispatch(createActionSetNietAfgeslotenVanaf(nietAfgeslotenVanaf));
                if (nietAfgeslotenVanaf !== undefined && nietAfgeslotenVanaf !== null && nietAfgeslotenVanaf !== store.getState().daglijstDatum) {
                    fetchApiPromise('GET', 'daglijst/geforceerd/' + nietAfgeslotenVanaf).then((response) => {
                        response.json().then(function(daglijstMetMutaties) {
                            if (daglijstMetMutaties) {
                                vulAfspraken(nietAfgeslotenVanaf, null, JSON.parse(daglijstMetMutaties.daglijstJson), daglijstMetMutaties.mutatieJsons);
                            }
                        });
                    });
                    persistentErrorToast('De werkdagen vanaf ' + datumFormaat(nietAfgeslotenVanaf) + ' zijn niet afgesloten. Sluit deze af via het dagverslag.');
                }
                return dagverslag;
            });
        },
    );
};
