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
import {AfspraakZoekFilter} from "../../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage"

export type KandidaatAfspraak = {
	capaciteitBlokId: number,
	datumTijd: Date,
	standplaatsPeriodeId: number,
	afstand: number,
	adres: string,
	postcode: string,
	plaats: string,
	clientEmailAdres: string,
	clientMobielNummer: string,
	toonBevestigingsBriefOptie: boolean,
	toonSmsHerinneringOptie: boolean,
	filter: AfspraakZoekFilter
}

export type AfspraakZoekResultaten = KandidaatAfspraak[]

export const geenAfspraakZoekResultaten = [] as AfspraakZoekResultaten;
