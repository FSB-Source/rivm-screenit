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
import {ColonDossier, leegColonDossier} from "../../datatypes/ColonDossier"
import {
    ColonDossierActions,
    HUIDIGE_INTAKE_AFSPRAAK,
    RESET_HERAANMELDENOPTIES,
    RESET_HUIDIGE_FIT_STATUS,
    SET_COLON_DOSSIER,
    SET_COLON_HUISARTS_HUIDIGE_RONDE,
    SET_COLON_HUISARTS_VORIGE_RONDE,
} from "../../actions/ColonDossierAction"

function colonDossierReducer(stateSlice: ColonDossier = leegColonDossier, action: ColonDossierActions): ColonDossier {
    switch (action.type) {
        case SET_COLON_DOSSIER:
            return {
                ...stateSlice,
                ...action.dossier,
                isInSync: true
            }
        case SET_COLON_HUISARTS_HUIDIGE_RONDE:
            return {
                ...stateSlice,
                huisartsHuidigeRonde: action.huisarts,
            }
        case SET_COLON_HUISARTS_VORIGE_RONDE:
            return {
                ...stateSlice,
                huisartsVorigeRonde: action.huisarts,
            }
        case RESET_HUIDIGE_FIT_STATUS:
            return {
                ...stateSlice,
                fitStatus: action.fitStatus,
            }
        case HUIDIGE_INTAKE_AFSPRAAK:
            return {
                ...stateSlice,
                intakeAfspraak: action.colonIntakeAfspraakDto,
            }
        case RESET_HERAANMELDENOPTIES:
            return {
                ...stateSlice,
                heraanmeldenOpties: action.heraanmeldenOpties,
            }
        default:
            return stateSlice
    }
}

export default colonDossierReducer
