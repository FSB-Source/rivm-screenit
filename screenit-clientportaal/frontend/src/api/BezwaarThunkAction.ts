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
import {BezwaarMoment} from "../datatypes/Bezwaar"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {ResetBezwaarMoment, setLaatsteBezwaarMomentAction} from "../actions/BezwaarReduxAction"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import HttpStatus from "../datatypes/HttpStatus"
import {navigateAndShowToast} from "../utils/NavigationUtil"
import {getBvoBaseUrl} from "../utils/UrlUtil"
import {getString} from "../utils/TekstPropertyUtil"
import properties from "./../pages/bvo/BezwaarType.json"

export const getLaatsteBezwaarMoment = (bvo: Bevolkingsonderzoek) => async (dispatch: Dispatch<ResetBezwaarMoment>) => {
    return ScreenitBackend.get(`/bezwaar/${bvo}`).then(
        (response) => dispatch(setLaatsteBezwaarMomentAction(response.data)),
    )
}

export const saveNieuwBezwaarMoment = (bvo: Bevolkingsonderzoek, bezwaarMoment: BezwaarMoment) => {
    return (dispatch: Dispatch) => {
        return ScreenitBackend.post(`/bezwaar/${bvo}`, bezwaarMoment)
            .then(response => {
                dispatch(setLaatsteBezwaarMomentAction(response.data))
                navigateAndShowToast(getBvoBaseUrl(bvo), getString(properties.toast.wijzigingen.titel), getString(properties.toast.wijzigingen.text))
            })
            .catch(error => {
                if (error.response.status === HttpStatus.NOT_MODIFIED) {
                    navigateAndShowToast(getBvoBaseUrl(bvo), getString(properties.toast.ongewijzigd.titel),
                        getString(properties.toast.ongewijzigd.text), ToastMessageType.WARNING)
                }
            })
    }
}
