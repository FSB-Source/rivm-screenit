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

import {baseUrl, createClientHeaders} from '../util/ApiUtil';
import {persistentSuccessToast} from '../util/ToastUtil';

export const verstuurAdHocVerzoek = (reden: string, afspraakId: number): void => {
    fetch(baseUrl + 'adhocMeekijkverzoek/indienen/' + afspraakId, {
        method: 'POST',
        headers: createClientHeaders(),
        body: reden,
    });
    persistentSuccessToast('Meekijkverzoek LRCB verzonden met reden: ' + reden);
};

export const magAdhocVersturen = (afspraakId: number): Promise<number> => {
    return fetch(baseUrl + 'adhocMeekijkverzoek/controleren/' + afspraakId, {
        method: 'POST',
        headers: createClientHeaders(),
    }).then(response => {
        return response.status;
    }).catch(response => {
            return 500;
        },
    );
};
