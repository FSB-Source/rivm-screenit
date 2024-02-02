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
import {CervixDossier} from "../datatypes/CervixDossier"
import {ZasStatus} from "../datatypes/cervix/ZasStatus"
import {CervixUitstelStatus} from "../datatypes/cervix/CervixUitstelStatus"
import {CervixUitstelDto} from "../datatypes/cervix/CervixUitstelDto"

export type CervixDossierActions =
    CervixDossierAction
    | ResetZasStatusAction
    | ResetCervixUitstelStatusAction
    | CervixUitstelAction

export const CREATE_CERVIX_DOSSIER = 'CREATE_CERVIX_DOSSIER';
export type CervixDossierAction = { type: typeof CREATE_CERVIX_DOSSIER, dossier: CervixDossier }
export const createCervixDossierAction = (dossier: CervixDossier): CervixDossierAction => ({
    type: CREATE_CERVIX_DOSSIER,
    dossier: dossier
});

export const RESET_HUIDIGE_ZAS_STATUS = "RESET_HUIDIGE_ZAS_STATUS"
export type ResetZasStatusAction = { type: typeof RESET_HUIDIGE_ZAS_STATUS, zasStatus: ZasStatus }
export const setHuidigeZasStatusAction = (zasStatus: ZasStatus): ResetZasStatusAction => ({
    type: RESET_HUIDIGE_ZAS_STATUS,
    zasStatus: zasStatus,
})

export const RESET_CERVIX_UITSTEL_STATUS = 'RESET_CERVIX_UITSTEL_STATUS';
export type ResetCervixUitstelStatusAction = { type: typeof RESET_CERVIX_UITSTEL_STATUS, cervixUitstelStatus: CervixUitstelStatus }
export const setCervixUitstelStatusAction = (cervixUitstelStatus: CervixUitstelStatus): ResetCervixUitstelStatusAction => ({
    type: RESET_CERVIX_UITSTEL_STATUS,
    cervixUitstelStatus: cervixUitstelStatus
});

export const CERVIX_UITSTEL = 'CERVIX_UITSTEL';
export type CervixUitstelAction = { type: typeof CERVIX_UITSTEL, cervixUitstelDto: CervixUitstelDto }
export const createCervixUitstelAction = (cervixUitstelDto: CervixUitstelDto): CervixUitstelAction => ({
    type: CERVIX_UITSTEL,
    cervixUitstelDto: cervixUitstelDto
})
