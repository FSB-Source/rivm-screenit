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

import type {ConnectieStatusLevel} from '../datatypes/connectiestatus/ConnectieStatus';

export type ConnectieStatusActions = PutMammograafConnectieStatusAction | PutIMSConnectieStatusAction;

export const PUT_MAMMOGRAAF_CONNECTIE_STATUS = 'PUT_MAMMOGRAAF_CONNECTIE_STATUS';
export type PutMammograafConnectieStatusAction = { type: 'PUT_MAMMOGRAAF_CONNECTIE_STATUS', aeTitle: string, statusLevel: ConnectieStatusLevel };
export const createActionPutMammograafConnectieStatus = (aeTitle: string, statusLevel: ConnectieStatusLevel): PutMammograafConnectieStatusAction => ({
    type: PUT_MAMMOGRAAF_CONNECTIE_STATUS,
    aeTitle: aeTitle,
    statusLevel: statusLevel,
});

export const PUT_IMS_CONNECTIE_STATUS = 'PUT_IMS_CONNECTIE_STATUS';
export type PutIMSConnectieStatusAction = { type: 'PUT_IMS_CONNECTIE_STATUS', statusLevel: ConnectieStatusLevel };
export const createActionPutIMSConnectieStatus = (statusLevel: ConnectieStatusLevel): PutIMSConnectieStatusAction => ({
    type: PUT_IMS_CONNECTIE_STATUS,
    statusLevel: statusLevel,
});
