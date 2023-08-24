/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
export function concatWithSpace(...args: any[]): string {
    return args.filter(Boolean).join(" ")
}

export function placeNonBreakingSpaceInDate(value: string) {
    const gesplitteString: string[] = value.split(" ")
    return gesplitteString[0] + " " + gesplitteString[1] + "&nbsp;" + gesplitteString[2]
}

export function splitAdresString(adres?: string) {
    const gesplitteAdres = adres ? adres.split(",", 2) : ""
    return gesplitteAdres[0] + "<br>" + gesplitteAdres[1]
}
