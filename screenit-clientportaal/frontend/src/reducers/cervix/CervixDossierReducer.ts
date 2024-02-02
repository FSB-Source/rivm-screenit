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
import {CervixDossier, leegCervixDossier} from "../../datatypes/CervixDossier"
import {
    CERVIX_UITSTEL,
    CervixDossierActions,
    CREATE_CERVIX_DOSSIER,
    RESET_CERVIX_UITSTEL_STATUS,
    RESET_HUIDIGE_ZAS_STATUS,
} from "../../actions/CervixDossierAction"

function cervixDossierReducer(stateSlice: CervixDossier = leegCervixDossier, action: CervixDossierActions): CervixDossier {
    switch (action.type) {
        case CREATE_CERVIX_DOSSIER:
            return {
                ...stateSlice,
                ...action.dossier,
                isInSync: true,
            }
        case RESET_HUIDIGE_ZAS_STATUS:
            return {
                ...stateSlice,
                zasStatus: {
                    ...action.zasStatus,
                    isInSync: true,
                },
            }
        case RESET_CERVIX_UITSTEL_STATUS:
            return {
                ...stateSlice,
                uitstelStatus: action.cervixUitstelStatus,
            }
        case CERVIX_UITSTEL:
            return {
                ...stateSlice,
                uitstel: {
                    ...action.cervixUitstelDto,
                    isInSync: true,
                },
            }
        default:
            return stateSlice
    }
}

export default cervixDossierReducer
