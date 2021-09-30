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

export type ErrorType = 'verplicht' | 'getal' | 'ongeldig';

export type ValidationError = {
    type: ErrorType;
    label: string;
}

const ongeldigErrorMessage = (label: string): string => {
    return label + ' is niet geldig.';
};

const requiredMessage = (label: string): string => {
    return label + ' is verplicht.';
};

const numberErrorMessage = (label: string): string => {
    return label + ' moet een heel getal zijn.';
};

export const getErrorMessage = (error: ValidationError): string => {
    switch (error.type) {
        case 'verplicht':
            return requiredMessage(error.label);
        case 'getal':
            return numberErrorMessage(error.label);
        case 'ongeldig':
            return ongeldigErrorMessage(error.label);
        default:
            return '';
    }
};
