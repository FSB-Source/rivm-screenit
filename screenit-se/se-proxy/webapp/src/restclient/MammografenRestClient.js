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

import {store} from '../Store';
import {fetchApiPromise} from '../util/ApiUtil';
import {createActionSetHuidigeMammograaf, createActionVulMammografen} from '../actions/MammograafActions';
import {Mammograaf} from '../datatypes/Mammograaf';
import {persistentErrorToast} from '../util/ToastUtil';

export const readMammografen = (): Promise<Array<Mammograaf>> => {
    return new Promise((resolve, reject) => {
        fetchApiPromise('GET', 'mammografen/').then(response => {
            response.json().then(mammografen => {
                if (!mammografen.length) {
                    persistentErrorToast('Voor deze SE is geen mammograaf geadministreerd, neem contact op met een beheerder.');
                }
                store.dispatch(createActionVulMammografen(mammografen));
                if (store.getState().environmentInfo.environment !== 'Test') {
                    const huidigWerkstationIpAdres: string = store.getState().environmentInfo.huidigWerkstationIpAdres;
                    const mammograaf: ?Mammograaf = mammografen.find((m: Mammograaf) => m.werkstationIpAdres === huidigWerkstationIpAdres);
                    if (mammograaf) {
                        store.dispatch(createActionSetHuidigeMammograaf(mammograaf.id));
                    }
                }
                resolve(mammografen);
            });
        }).catch(error => reject(error));
    });
};
