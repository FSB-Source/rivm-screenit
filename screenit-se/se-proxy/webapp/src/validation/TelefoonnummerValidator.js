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

import type {Validation} from './Validation';
import type {ValidationError} from '../datatypes/Error';
import {getErrorMessage} from '../datatypes/Error';

export class TelefoonnummerValidator<T: ?string> implements Validation<T> {

    isValid(value: ?string): boolean {
        return isTelefoonnummerValid(value);
    }

    getErrorMessage(value: ?string, fieldLabel: string): string {
        let errors: ValidationError[] = validateTelefoonnummer(value, fieldLabel);
        if (errors.length > 0) {
            return getErrorMessage(errors[0]);
        }
        return '';
    }

}

function isTelefoonnummerValid(telefoonnummer: ?string): boolean {
    return !telefoonnummer || telefoonnummerCheck(telefoonnummer);
}

function telefoonnummerCheck(telefoonnummer: ?string): boolean {
    return telefoonnummer ?
        (isVastNlNummer(telefoonnummer) || isMobielNlNummer(telefoonnummer) || isInformatieNlNummer(telefoonnummer) || isBuitenlandsNummer(telefoonnummer)) :
        true;
}

function isVastNlNummer(telefoonnummer: string): boolean {
    return exactMatch(telefoonnummer, /^(0[0-9]{9})|(0[0-9]{2}( |-)[0-9]{7})|(0[0-9]{3}( |-)[0-9]{6})$/);
}

function isMobielNlNummer(telefoonnummer: string): boolean {
    return exactMatch(telefoonnummer, /^(06( |-)?[0-9]{8})$/);
}

function isInformatieNlNummer(telefoonnummer: string): boolean {
    return exactMatch(telefoonnummer, /^(0(8|9)00( |-)?\d{4}(\d{3})?$)$/);
}

function isBuitenlandsNummer(telefoonnummer: string): boolean {
    return exactMatch(telefoonnummer, /^(\+|00)[0-9 -]{4,15}$/);
}

function exactMatch(telefoonnummer: string, regex: RegExp): boolean {
    const regexResult = regex.exec(telefoonnummer);
    if (regexResult) {
        return regexResult[0].length === telefoonnummer.length;
    } else {
        return false;
    }
}

function validateTelefoonnummer(telefoonnummer: ?string, label: string): ValidationError[] {
    let errors: ValidationError[] = [];

    validateTelefoonnummerOngeldigError(telefoonnummer, label, errors);

    return errors;
}

function validateTelefoonnummerOngeldigError(telefoonnummer, label, errors): void {
    if (!isTelefoonnummerValid(telefoonnummer)) {
        errors.push({
            type: 'ongeldig',
            label: label,
        });
    }
}
