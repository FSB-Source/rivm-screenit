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

import moment from 'moment';

export const connectieStatusLevels = {
    OK: 'fa fa-check-circle',
    WARN: 'fa fa-exclamation-triangle',
    FAULT: 'fa fa-exclamation-circle',
};

export const getMostCriticalStatusLevel = (connectieStatusses: Array<ConnectieStatusLevel>): ConnectieStatusLevel => {
    const highestStatusIndex = Math.max(...connectieStatusses.map(statusType => Object.keys(connectieStatusLevels).findIndex((c) => c === statusType)));
    return Object.keys(connectieStatusLevels)[highestStatusIndex];
};

export type ConnectieStatusLevel = $Keys<typeof connectieStatusLevels>;

export type ConnectieStatus = {
    mammograafConnectieStatusByAeTitle: Map<string, ConnectieStatusLevel>;
    imsConnectieStatus: ConnectieStatusLevel;
    imsConnectieStatusTimestamp: ?string;
}
