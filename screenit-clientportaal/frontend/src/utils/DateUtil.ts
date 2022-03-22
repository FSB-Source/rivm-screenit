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
import {addBusinessDays, addDays, addMonths, max, min, startOfDay, subDays, subMonths} from "date-fns"
import {isNullOfUndefined} from "./EmptyUtil"
import {cpStore} from "../index"

export const berekenOffset = (datumTijd: Date): number => {
	const lokaalDatum = new Date()
	return lokaalDatum.getTime() - datumTijd.getTime()
}

export const nu = (): Date => {
	return new Date(new Date().getTime() - (!isNullOfUndefined(cpStore) && !isNullOfUndefined(cpStore.getState()) ? cpStore.getState().environmentInfo.dateTimeOffset : 0))
}

export const parseIsoDatumUitDto = (isoString: string): Date => {
	return new Date(isoString)
}

export const vandaag = (): Date => {
	return beginDag(nu())
}

export const beginDag = (datum: Date): Date => {
	return startOfDay(datum)
}

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

export const plusDagen = (datum: Date, dagen: number): Date => {
	return addDays(datum, dagen)
}

export const minDagen = (datum: Date, dagen: number): Date => {
	return subDays(datum, dagen)
}

export const plusMaanden = (datum: Date, maanden: number): Date => {
	return addMonths(datum, maanden)
}

export const minMaanden = (datum: Date, maanden: number): Date => {
	return subMonths(datum, maanden)
}

export const isDatumVoorVandaag = (datum: Date): boolean => {
	return beginDag(datum).getTime() < vandaag().getTime()
}

export const isDatumVandaagOfLater = (datum: Date): boolean => {
	return beginDag(datum).getTime() >= vandaag().getTime()
}

export const isDatumInToekomst = (datum: Date): boolean => {
	return datum > nu()
}

export const geefLaatsteDatum = (date1: Date, date2: Date): Date => {
	return date1 >= date2 ? date1 : date2
}

export const plusWerkdagen = (datum: Date, dagen: number): Date => {
	return addBusinessDays(datum, dagen)
}

export function getOndergrensUitLijst(lijst: Date[] | undefined): Date | undefined {
	return lijst ? min(lijst) : undefined
}

export function getBovengrensUitLijst(lijst: Date[] | undefined): Date | undefined {
	return lijst ? max(lijst) : undefined
}

export function lijstBevatMeegegevenDatum(lijst: Date[] | undefined, value: Date): boolean {
	return !!lijst && lijst.some(date => +date === +value)
}

export function zoekIndex(lijst: Date[] | undefined, value: Date | undefined): number {
	return (!!lijst && !!value) ? lijst.findIndex((d) => d.getTime() === value.getTime()) : -1
}

const getDateTimeFormat = (options: Intl.DateTimeFormatOptions): Intl.DateTimeFormat => {
	return new Intl.DateTimeFormat("nl-NL", {
		...options,
		timeZone: "Europe/Amsterdam",
	})
}
