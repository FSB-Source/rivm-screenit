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
import {createActionVulPlanning} from '../actions/PlanningActions';
import type {PlanningDto} from '../datatypes/Planning';
import {GeenScreeningBlok} from '../datatypes/Planning';
import {getDate, getTime} from '../util/DateUtil';

const geenScreeningBlokFromDto = (geenScreeningblokDto): GeenScreeningBlok => {
    const geenScreeningblok: GeenScreeningBlok = new GeenScreeningBlok();

    geenScreeningblok.vanafDatum = getDate(geenScreeningblokDto.vanaf);
    geenScreeningblok.vanafTijd = getTime(geenScreeningblokDto.vanaf);
    geenScreeningblok.totDatumTijd = geenScreeningblokDto.tot;
    geenScreeningblok.opmerking = geenScreeningblokDto.opmerking;

    return geenScreeningblok;
};

export const leesPlanning = (datum: string): void => {
    fetchApiPromise('GET', 'planning/' + datum).then(
        (response) => {
            if (response.ok) {
                response.json().then((planningDto: PlanningDto) => {
                    store.dispatch(createActionVulPlanning(datum,
                        {
                            geenScreeningBlokken: planningDto.geenScreeningBlokken.map(geenScreeningBlokFromDto),
                        },
                    ));
                });
            } else {
                console.warn('LeesPlanning: geen ok response maar: ' + response.status);
            }
        });
};
