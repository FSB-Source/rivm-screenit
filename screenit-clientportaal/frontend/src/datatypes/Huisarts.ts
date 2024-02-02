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
import {Adres, AdresZoekobject} from "./adres/Adres"

export type Huisarts = {
    id: number
    achternaam?: string
    voorletters?: string
    tussenvoegsels?: string
    adres: Adres
    praktijknaam: string
    telefoonnummer?: string
}

export type HuisartsZoekobject = {
    naam?: string
    adres: AdresZoekobject
}

export enum MammaGeenHuisartsOptie {
    HUISARTS_IN_HET_BUITENLAND,
    CLIENT_WIL_HUISARTS_NIET_OPGEVEN,
    TEHUIS_HUISARTS,
    HUISARTS_STAAT_ER_NIET_TUSSEN,
}

export type HuisartsZoekresultaten = Huisarts[];

export const geenHuisartsZoekresultaten = [] as HuisartsZoekresultaten

export const leegHuisartsZoekobject = {
    adres: {} as AdresZoekobject,
}
