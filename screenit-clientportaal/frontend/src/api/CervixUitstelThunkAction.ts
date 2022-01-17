/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {CervixUitstelDto, CervixUitstelFormulier} from "../datatypes/cervix/CervixUitstelDto"
import {plusMaanden} from "../utils/DateUtil"
import {CervixUitstelAction, createCervixUitstelAction, ResetCervixUitstelStatusAction, setCervixUitstelStatusAction} from "../actions/CervixDossierAction"
import {CervixUitstelType} from "../datatypes/cervix/CervixUitstelType"

export const getHuidigeCervixUitstelStatus = () => async (dispatch: Dispatch<ResetCervixUitstelStatusAction>) => {
    return ScreenitBackend.get("/cervix/uitstellen/status")
        .then(response => dispatch(setCervixUitstelStatusAction(response.data)))
}

export const getHuidigeCervixUitstel = () => async (dispatch: Dispatch<CervixUitstelAction>) => {
    return ScreenitBackend.get(`/cervix/uitstellen/huidig`)
        .then(response => dispatch(createCervixUitstelAction(response.data)))
}

export const saveCervixUitstel = (cervixUitstel: CervixUitstelFormulier, uitstelBijZwangerschap: number) => (dispatch: Dispatch) => {

    const cervixUitstelDto: CervixUitstelDto = {
        uitstelType: cervixUitstel.uitstelType,
        uitstellenTotDatum: getUitstellenTotDatum(cervixUitstel, uitstelBijZwangerschap),
    }

    return ScreenitBackend.put("/cervix/uitstellen/aanvragen", cervixUitstelDto)
        .then(response => {
            dispatch(createCervixUitstelAction(response.data))
        })
}

export function getUitstellenTotDatum(cervixUitstel: CervixUitstelFormulier, uitstelBijZwangerschap: number): Date | null {
	return (cervixUitstel.uitstelType === CervixUitstelType.ZWANGERSCHAP && cervixUitstel.uitgerekendeDatum) ? plusMaanden(cervixUitstel.uitgerekendeDatum, uitstelBijZwangerschap) : cervixUitstel.uitstellenTotDatum
}
