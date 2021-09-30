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

import moment, {Duration} from 'moment';
import localization from 'moment/locale/nl';
import {createActionKiesDaglijstDatum} from '../actions/DaglijstDatumActions';
import {store} from '../Store';
import {leesAfspraken} from '../restclient/DaglijstRestclient';
import {leesPlanning} from '../restclient/PlanningRestClient';
import {createActionNavigateToDaglijst} from '../actions/NavigationActions';

moment.updateLocale('nl', localization);

export const vandaagISO = (): string => nu().format('YYYY-MM-DD');

let offset = 0;
export const setOffset = (val: Duration) => {
    offset = val;
    if (store.getState().session) {
        store.dispatch(createActionKiesDaglijstDatum(vandaagISO()));
        leesAfspraken(vandaagISO(), createActionNavigateToDaglijst());
        leesPlanning(vandaagISO());
    }
};

export const nu = () => {
    if (offset !== 0) {
        let nuDST = moment().isDST();
        let goal = moment().add(offset);
        let goalDST = goal.isDST();
        if (nuDST && !goalDST) {
            goal.add(moment.duration({'s': 3600}));
        } else if (!nuDST && goalDST) {
            goal.subtract(moment.duration({'s': 3600}));
        }
        return goal;
    }
    return moment();
};

export const nuISO = (): string => nu().format('YYYY-MM-DDTHH:mm:ss');

export const nuTimestamp = () => nu().format('HH:mm:ss.SSS');

export const nuTijdUrenMinuten = () => nu().format('HH:mm');

export const ligtTussenData = (datum: string, startDatum: ?string, eindDatum: ?string) => {
    if (startDatum || eindDatum) {
        return (startDatum ? getDate(startDatum) <= datum : true)
            && (eindDatum ? datum <= getDate(eindDatum) : true);
    } else {
        return false;
    }
};

export const datumFormaat = (isoDatum: ?string): string => isoDatum ? moment(isoDatum).format('DD-MM-YYYY') : '';

export const getDate = (isoDatetime: string) => {
    return isoDatetime.split('T')[0];
};

export const getTime = (isoDatetime: string) => {
    return (isoDatetime.split('T')[1]).slice(0, 5);
};

export const datumInVerleden = (isoDateTime: string) => {
    const datum = new Date(isoDateTime);
    const gisteren = nu().subtract(1, 'days').toDate();
    return datum <= gisteren;
};

export const datumInToekomst = (isoDateTime: string) => {
    const datum = new Date(isoDateTime);
    const vandaagIso = nu().toDate();
    return datum > vandaagIso;
};

export const getTijdGeledenTekst = (timestamp: string) => {
    return moment(timestamp).fromNow();
};
