/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {addBusinessDays, addDays, addMonths, differenceInDays, format, isWeekend, max, min, startOfDay, subDays, subMonths} from "date-fns"
import {isNullOfUndefined} from "./EmptyUtil"
import {cpStore} from "../index"
import {nl} from "date-fns/locale"

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

export const parseIsoDatumNederlandseIso = (isoString: string): Date => {
	const split = isoString.split("-")
	if (split[2] === undefined && split[1] === undefined) {
		return new Date(split[0])
	}
	if (split[2] === undefined) {
		return new Date(+split[1], +split[0] - 1)
	}
	return new Date(+split[2], +split[1] - 1, +split[0])
}

export const vandaag = (): Date => {
	return beginDag(nu())
}

export const beginDag = (datum: Date): Date => {
	return startOfDay(datum)
}

export const formatDate = (datum?: Date): string => {
	if (!datum) {
		return ""
	}

	return format(datum, "dd-MM-yyyy", {locale: nl})
}

export const formatTime = (datum?: Date): string => {
	if (!datum) {
		return ""
	}

	return format(datum, "HH:mm", {locale: nl})
}

export const formatDateText = (datum?: Date | null): string => {
	if (!datum) {
		return ""
	}

	return format(datum, "d MMMM yyyy", {locale: nl})
}

export const formatDateWithDayName = (datum?: Date | null): string => {
	if (!datum) {
		return ""
	}

	return format(datum, "EEEE d MMMM", {locale: nl})
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

export const isWerkdag = (datum: Date): boolean => {
	return !isWeekend(datum)
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

export function getAantalDagenTussenDatums(eersteDatum: Date, tweedeDatum: Date) {
	return differenceInDays(eersteDatum, tweedeDatum)
}
