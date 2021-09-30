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

import type {ValidationError} from '../datatypes/Error';
import {getErrorMessage} from '../datatypes/Error';
import type {Validation} from './Validation';

export class EmailValidator<T: ?string> implements Validation<T> {

    isValid(value: ?string): boolean {
        return isEmailadresValid(value);
    }

    getErrorMessage(value: ?string, fieldLabel: string): string {
        let errors: ValidationError[] = validateEmaildres(value, fieldLabel);
        if (errors.length > 0) {
            return getErrorMessage(errors[0]);
        }
        return '';
    }

}

function isEmailadresValid(email: ?string): boolean {
    if (email) {
        return /^(([a-zA-Z0-9_\-.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(]?)(\s*(;|,)\s*|\s*$))*$/.test(email);
    }
    return true;
}

function validateEmaildres(email: ?string, label: string): ValidationError[] {
    let errors: ValidationError[] = [];

    validateEmailadresOngeldigError(email, label, errors);

    return errors;
}

function validateEmailadresOngeldigError(email, label, errors): void {
    if (!isEmailadresValid(email)) {
        errors.push({
            type: 'ongeldig',
            label: label,
        });
    }
}
