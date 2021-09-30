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

import type {FhirUserSession} from '../datatypes/ims/FhirUserSession';
import type {FhirUser} from '../datatypes/ims/FhirUser';
import type {FhirImagingStudy} from '../datatypes/ims/FhirImagingStudy';

const createUser = (username: string): FhirUser => {
    return {
        identifier: {
            system: 'ScreenIT',
            value: username,
            type: {
                coding: {
                    code: 'Mbb',
                    system: 'ScreenIT',
                },
            },
        },
    };
};

const createImagingStudy = (uitnodigingsNr: number | string): FhirImagingStudy => {
    return {
        identifier: {
            system: 'ScreenIT',
            value: uitnodigingsNr,
        },
        accession: {
            system: 'ScreenIT',
            value: uitnodigingsNr,
        },
    };
};

const createFocus = (bsn: string, uitnodigingsNr: number | string) => {
    return {
        patient: {
            identifier: {
                system: 'NLMINBIZA',
                value: bsn,
            },
        },
        imagingStudy: createImagingStudy(uitnodigingsNr),
    };
};

export const createStudyIms = (uitnodigingsNr: number, bsn: string, username: string): FhirUserSession => {
    return {
        resourceType: 'UserSession',
        user: createUser(username),
        focus: createFocus(bsn, uitnodigingsNr),
        context: {
            worklist: [],
            type: 'Worklist',
            value: 'UpcomingCases',
        },
    };
};

export const createEmptyStudyIms = (username: string): FhirUserSession => {
    return {
        resourceType: 'UserSession',
        user: createUser(username),
        focus: createFocus('', ''),
        context: {
            worklist: [],
            type: 'Worklist',
            value: 'UpcomingCases',
        },
    };
};

export const createLogonIms = (username: string): FhirUserSession => {
    return {
        resourceType: 'UserSession',
        user: createUser(username),
        context: {
            type: 'Session',
            value: 'LogOn',
        },
    };
};

export const createLogoffIms = (username: string): FhirUserSession => {
    return {
        resourceType: 'UserSession',
        user: createUser(username),
        context: {
            type: 'Session',
            value: 'LogOff',
        },
    };
};
