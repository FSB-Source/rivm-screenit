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

export class BsnValidator<T: ?string> implements Validation<T> {

    isValid(value: ?string): boolean {
        return isBsnValid(value);
    }

    getErrorMessage(value: ?string, fieldLabel: string): string {
        let errors: ValidationError[] = validateBsn(value, fieldLabel);
        if (errors.length > 0) {
            return getErrorMessage(errors[0]);
        }
        return '';
    }

}

function isBsnValid(bsn: ?string): boolean {
    if (bsn && bsn.length === 9) {
        const check = (parseInt(bsn[0], 10) * 9) +
            (parseInt(bsn[1], 10) * 8) +
            (parseInt(bsn[2], 10) * 7) +
            (parseInt(bsn[3], 10) * 6) +
            (parseInt(bsn[4], 10) * 5) +
            (parseInt(bsn[5], 10) * 4) +
            (parseInt(bsn[6], 10) * 3) +
            (parseInt(bsn[7], 10) * 2) +
            (parseInt(bsn[8], 10) * -1);
        return (check % 11 === 0);
    }
    return false;
}

function validateBsn(email: ?string, label: string): ValidationError[] {
    let errors: ValidationError[] = [];

    validateBsnOngeldigError(email, label, errors);

    return errors;
}

function validateBsnOngeldigError(bsn, label, errors): void {
    if (!isBsnValid(bsn)) {
        errors.push({
            type: 'ongeldig',
            label: label,
        });
    }
}
