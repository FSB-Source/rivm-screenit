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
import {ColonDossier} from "../datatypes/ColonDossier"
import {Huisarts} from "../datatypes/Huisarts"
import {FitStatus} from "../datatypes/colon/FitStatus"
import {ColonIntakeAfspraakDto} from "../datatypes/colon/ColonIntakeAfspraakDto"
import {HeraanmeldenOptiesDto} from "../datatypes/afmelden/HeraanmeldenOptiesDto"

export type ColonDossierActions =
    ColonDossierAction
    | SetColonHuisartsVorigeRonde
    | SetColonHuisartsHuidigeRonde
    | ResetFitStatus
    | ColonIntakeAfspraakAction
    | ResetHeraanmeldenOpties

export const SET_COLON_DOSSIER = "SET_COLON_DOSSIER"
export type ColonDossierAction = { type: typeof SET_COLON_DOSSIER, dossier: ColonDossier }
export const createColonDossierAction = (dossier: ColonDossier): ColonDossierAction => ({
    type: SET_COLON_DOSSIER,
    dossier: dossier,
})

export type SetColonHuisartsVorigeRonde = { type: typeof SET_COLON_HUISARTS_VORIGE_RONDE, huisarts: Huisarts | undefined }
export const SET_COLON_HUISARTS_VORIGE_RONDE = "SET_COLON_HUISARTS_VORIGE_RONDE"
export const setColonHuisartsVorigeRondeReduxAction = (huisarts: Huisarts | undefined): SetColonHuisartsVorigeRonde => ({
    type: SET_COLON_HUISARTS_VORIGE_RONDE,
    huisarts: huisarts,
})

export const SET_COLON_HUISARTS_HUIDIGE_RONDE = "SET_COLON_HUISARTS_HUIDIGE_RONDE"
export type SetColonHuisartsHuidigeRonde = { type: typeof SET_COLON_HUISARTS_HUIDIGE_RONDE, huisarts: Huisarts | undefined }
export const setColonHuisartsHuidigeRondeReduxAction = (huisarts: Huisarts | undefined): SetColonHuisartsHuidigeRonde => ({
    type: SET_COLON_HUISARTS_HUIDIGE_RONDE,
    huisarts: huisarts,
})

export const RESET_HUIDIGE_FIT_STATUS = "RESET_HUIDIGE_FIT_STATUS"
export type ResetFitStatus = { type: typeof RESET_HUIDIGE_FIT_STATUS, fitStatus: FitStatus }
export const setHuidigeFitStatusAction = (fitStatus: FitStatus): ResetFitStatus => ({
    type: RESET_HUIDIGE_FIT_STATUS,
    fitStatus: fitStatus,
})

export const HUIDIGE_INTAKE_AFSPRAAK = "SET_HUIDIGE_INTAKE_AFSPRAAK"
export type ColonIntakeAfspraakAction = { type: typeof HUIDIGE_INTAKE_AFSPRAAK, colonIntakeAfspraakDto: ColonIntakeAfspraakDto }
export const setColonIntakeAfspraakAction = (colonIntakeAfspraakDto: ColonIntakeAfspraakDto): ColonIntakeAfspraakAction => ({
    type: HUIDIGE_INTAKE_AFSPRAAK,
    colonIntakeAfspraakDto: colonIntakeAfspraakDto,
})

export const RESET_HERAANMELDENOPTIES = "RESET_HERAANMELDENOPTIES"
export type ResetHeraanmeldenOpties = { type: typeof RESET_HERAANMELDENOPTIES, heraanmeldenOpties: HeraanmeldenOptiesDto }
export const setHeraanmeldenOpties = (heraanmeldenOpties: HeraanmeldenOptiesDto): ResetHeraanmeldenOpties => ({
    type: RESET_HERAANMELDENOPTIES,
    heraanmeldenOpties: heraanmeldenOpties,
})
