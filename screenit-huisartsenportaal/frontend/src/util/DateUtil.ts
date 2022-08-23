/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
export const formatDate = (datum?: Date): string => {
	return !!datum ? getDateTimeFormat({
		year: "numeric", month: "2-digit", day: "2-digit",
	}).format(datum) : ""
}

export const formatTime = (datum?: Date): string => {
	return !!datum ? getDateTimeFormat({
		hour: "numeric", minute: "numeric",
	}).format(datum) : ""
}

export const formatDateText = (datum?: Date | null): string => {
	return !!datum ? getDateTimeFormat({
		year: "numeric", month: "long", day: "numeric",
	}).format(datum) : ""
}

export const formatDateWithDayName = (datum?: Date | null): string => {
	return !!datum ? getDateTimeFormat({
		month: "long", day: "numeric", weekday: "long",
	}).format(datum) : ""
}

export const formatDateTime = (datum?: Date | null): string => {
	return !!datum ? getDateTimeFormat({
		year: "numeric", month: "2-digit", day: "2-digit",
		hour: "numeric", minute: "numeric",
	}).format(datum) : ""
}

export const parseDate = (datum?: string | Date | null): Date | null => {
	if (!datum) {
		return null
	}
	const parsedDate = new Date(datum)
	return new Date(parsedDate.getTime() + parsedDate.getTimezoneOffset() * 60000)
}

const getDateTimeFormat = (options: Intl.DateTimeFormatOptions): Intl.DateTimeFormat => {
	return new Intl.DateTimeFormat("nl-NL", {
		...options,
		timeZone: "Europe/Amsterdam",
	})
}
