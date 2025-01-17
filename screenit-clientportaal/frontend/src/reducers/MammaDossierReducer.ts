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
import {
    MammaDossierActions,
    SET_HUIDIGE_MAMMA_AFSPRAAK,
    SET_MAMMA_DOSSIER,
    SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE,
    SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE,
    SET_MAMMA_HUISARTS_HUIDIGE_RONDE,
    SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN,
    SET_MAMMA_HUISARTS_VORIGE_RONDE,
} from "../actions/MammaDossierAction"
import {leegMammaDossier, MammaDossier} from "../datatypes/MammaDossier"

function mammaDossierReducer(stateSlice: MammaDossier = leegMammaDossier, action: MammaDossierActions): MammaDossier {
    switch (action.type) {
        case SET_MAMMA_DOSSIER:
            return {
                ...stateSlice,
                ...action.dossier,
                isInSync: true
            }
        case SET_MAMMA_HUISARTS_HUIDIGE_RONDE:
            return {
                ...stateSlice,
                huisartsHuidigeRonde: action.huisarts,
            }
        case SET_MAMMA_HUISARTS_VORIGE_RONDE:
            return {
                ...stateSlice,
                huisartsVorigeRonde: action.huisarts,
            }
        case SET_MAMMA_HUISARTS_MAG_ONTKOPPELEN:
            return {
                ...stateSlice,
                magHuisartsOntkoppelen: action.magOntkoppelen,
            }
        case SET_MAMMA_GEEN_HUISARTS_OPTIE_HUIDIGE_RONDE:
            return {
                ...stateSlice,
                geenHuisartsOptieHuidigeRonde: action.geenHuisartsOptie,
            }
        case SET_MAMMA_GEEN_HUISARTS_OPTIE_VORIGE_RONDE:
            return {
                ...stateSlice,
                geenHuisartsOptieVorigeRonde: action.geenHuisartsOptie,
            }
        case SET_HUIDIGE_MAMMA_AFSPRAAK:
            return {
                ...stateSlice,
                huidigeAfspraak: action.huidigeAfspraak,
            }
        default:
            return stateSlice
    }
}

export default mammaDossierReducer
