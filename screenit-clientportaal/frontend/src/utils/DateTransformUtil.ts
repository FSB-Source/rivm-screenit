/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {isNullOfUndefined} from "./EmptyUtil"
import {parseIsoDatumUitDto} from "./DateUtil"

export function transformDates(obj: any): any {
	if (!isNullOfUndefined(obj)) {
		if (typeof obj === "string" && isDate(obj)) {
			return parseIsoDatumUitDto(obj)
		} else if (Array.isArray(obj)) {
			return transformDatesInArray(obj)
		} else if (typeof obj === "object") {
			return transformDatesInObject(obj)
		}
	}
	return obj
}

function transformDatesInArray(obj: any[]): any {
	return obj.map((value) => {
		return transformDates(value)
	})
}

function transformDatesInObject(obj: any): any {
	return Object.entries(obj).reduce((result, [key, value]) => {
		result[key as any] = transformDates(value)
		return result
	}, obj)
}

function isDate(value: string): boolean {
	return /^([1-9][0-9]{3}-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]((:[0-5][0-9]\.[0-9]{3}Z)|((:[0-5][0-9](\.[0-9]{1,9})?)?\+[0-9]{2}:[0-9]{2}))?)$/.test(value)
}
