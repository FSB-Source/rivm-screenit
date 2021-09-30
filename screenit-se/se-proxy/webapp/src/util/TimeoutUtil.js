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
import {logoutClient} from '../restclient/AuthenticatieRestclient';
import {showErrorToast} from './ToastUtil';
import {nu} from './DateUtil';

let logoutMoment;
let idleCheck;

export const ensureIdleCheck = () => {
    if (idleCheck) {
        clearInterval(idleCheck);
    }
    resetTimeout();
    idleCheck = setInterval(timerIncrement, 1000);
};

export const resetTimeout = () => {
    logoutMoment = nu().add(30, 'minutes');
};

const timerIncrement = () => {
    if (store.getState().session !== null && store.getState().online && (nu() > logoutMoment)) {
        const yubikeyIdentificatie = store.getState().session.yubikeyIdentificatie;
        console.log('Uitloggen door 30 min inactiviteit: ' + yubikeyIdentificatie);
        logoutClient(yubikeyIdentificatie);
        showErrorToast('Uitgelogd wegens inactiviteit');
    }
};
