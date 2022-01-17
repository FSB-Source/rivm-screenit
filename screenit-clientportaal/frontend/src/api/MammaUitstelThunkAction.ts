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
import {createShowToastAction} from "../actions/ToastAction"
import {getString} from "../utils/TekstPropertyUtil"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {KandidaatStandplaatsPeriode} from "../datatypes/mamma/KandidaatStandplaatsPeriode"
import {navigateAndShowToast} from "../utils/NavigationUtil"
import {formatDateText} from "../utils/DateUtil"
import properties from "../pages/bvo/mamma/afspraak/MammaAfspraakUitstellenPage.json"

export const maakUitstel = (standplaatsPeriode: KandidaatStandplaatsPeriode) => (dispatch: Dispatch) => {
    return ScreenitBackend.post("/mamma/uitstel", standplaatsPeriode)
        .then(() => {
            navigateAndShowToast("/mamma",
                getString(properties.toast.title, [formatDateText(standplaatsPeriode.filter.vanaf)]),
                getString(properties.toast.description))

        })
        .catch(error => {
            dispatch(createShowToastAction({
                title: getString(properties.toast.error),
                description: error.response.data,
                type: ToastMessageType.ERROR,
            }))
            return Promise.reject()
        })
}
