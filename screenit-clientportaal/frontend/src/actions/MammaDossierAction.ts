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
import {MammaDossier} from "../datatypes/MammaDossier"
import {Huisarts, MammaGeenHuisartsOptie} from "../datatypes/Huisarts"
import {HuidigeAfspraak} from "../datatypes/mamma/HuidigeAfspraak"

export type MammaDossierActions =
    MammaDossierAction
    | SetMammaHuisartsHuidigeRonde
    | SetMammaHuisartsVorigeRonde
    | SetMagMammaHuisartsOntkoppelen
    | SetMammaGeenHuisartsOptieHuidigeRonde
    | SetMammaGeenHuisartsOptieVorigeRonde
    | SetHuidigeMammaAfspraak

export const SET_MAMMA_DOSSIER = "SET_MAMMA_DOSSIER"
export type MammaDossierAction = { type: typeof SET_MAMMA_DOSSIER, dossier: MammaDossier }
export const createMammaDossierAction = (dossier: MammaDossier): MammaDossierAction => ({
    type: SET_MAMMA_DOSSIER,
    dossier: dossier,
})

export const SET_MAMMA_HUISARTS_VORIGE_RONDE = "SET_MAMMA_HUISARTS_VORIGE_RONDE"
export type SetMammaHuisartsVorigeRonde = { type: typeof SET_MAMMA_HUISARTS_VORIGE_RONDE, huisarts: Huisarts | undefined }
export const setMammaHuisartsVorigeRondeReduxAction = (huisarts: Huisarts | undefined): SetMammaHuisartsVorigeRonde => ({
    type: SET_MAMMA_HUISARTS_VORIGE_RONDE,
    huisarts: huisarts,
})

export const SET_MAMMA_HUISARTS_HUIDIGE_RONDE = "SET_MAMMA_HUISARTS_HUIDIGE_RONDE"
export type SetMammaHuisartsHuidigeRonde = { type: typeof SET_MAMMA_HUISARTS_HUIDIGE_RONDE, huisarts: Huisarts | undefined }
export const setMammaHuisartsHuidigeRondeReduxAction = (huisarts: Huisarts | undefined): SetMammaHuisartsHuidigeRonde => ({
    type: SET_MAMMA_HUISARTS_HUIDIGE_RONDE,
    huisarts: huisarts,
})

export const SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN = "SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN"
export type SetMagMammaHuisartsOntkoppelen = { type: typeof SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN, magOntkoppelen: boolean }
export const setMammaGeenHuisartsOptieHuidigeRondeReduxAction = (geenHuisartsOptie: MammaGeenHuisartsOptie | undefined): SetMammaGeenHuisartsOptieHuidigeRonde => ({
    type: SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE,
    geenHuisartsOptie: geenHuisartsOptie,
})

export const SET_HUIDIGE_MAMMA_AFSPRAAK = "SET_HUIDIGE_MAMMA_AFSPRAAK"
export type SetHuidigeMammaAfspraak = { type: typeof SET_HUIDIGE_MAMMA_AFSPRAAK, huidigeAfspraak: HuidigeAfspraak | undefined }
export const setMammaGeenHuisartsOptieVorigeRondeReduxAction = (geenHuisartsOptie: MammaGeenHuisartsOptie | undefined): SetMammaGeenHuisartsOptieVorigeRonde => ({
    type: SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE,
    geenHuisartsOptie: geenHuisartsOptie,
})

export const SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE = "SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE"
export type SetMammaGeenHuisartsOptieHuidigeRonde = { type: typeof SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE, geenHuisartsOptie: MammaGeenHuisartsOptie | undefined }
export const magMammaHuisartsOntkoppelenReduxAction = (magOntkoppelen: boolean): SetMagMammaHuisartsOntkoppelen => ({
    type: SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN,
    magOntkoppelen: magOntkoppelen,
})

export const SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE = "SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE"
export type SetMammaGeenHuisartsOptieVorigeRonde = { type: typeof SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE, geenHuisartsOptie: MammaGeenHuisartsOptie | undefined }
export const setHuidigeMammaAfspraakReduxAction = (huidigeAfspraak: HuidigeAfspraak | undefined): SetHuidigeMammaAfspraak => ({
    type: SET_HUIDIGE_MAMMA_AFSPRAAK,
    huidigeAfspraak: huidigeAfspraak,
})
