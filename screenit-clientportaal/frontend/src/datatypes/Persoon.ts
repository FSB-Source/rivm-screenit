/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {Geslacht} from "./Geslacht"
import {TijdelijkAdres} from "./adres/TijdelijkAdres"
import {isNullOfLeeg} from "../utils/EmptyUtil"

export type Persoon = {
	id: number,
	aanhef: string,
	voorletters: string,
	aanspreekTussenvoegselEnAchternaam: string,
	bsn: string,
	geboortedatumDisplay: string,
	geslacht: Geslacht,
	adresTekst: string,
	emailadres: string,
	tijdelijkAdresTekst: string,
	tijdelijkAdres?: TijdelijkAdres,
	telefoonnummer1: string,
	telefoonnummer2: string,
	vertrokkenUitNederland?: boolean

}

export const heeftTelefoonnummer = (persoon: Persoon): boolean => {
	return (!isNullOfLeeg(persoon.telefoonnummer1))
		|| (!isNullOfLeeg(persoon.telefoonnummer2))
}

export const heeftAanspreekVorm = (persoon: Persoon): boolean => {
	return (!isNullOfLeeg(persoon.aanhef))
}

export const legePersoon = {} as Persoon
