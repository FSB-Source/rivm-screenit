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
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {createShowToastAction} from "../actions/ToastAction"
import {getString} from "../utils/TekstPropertyUtil"
import properties from "../pages/bvo/cervix/ZasAanvragenPage.json"
import {AxiosResponse} from "axios"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import HttpStatusCode from "../datatypes/HttpStatus"
import {ResetZasStatusAction, setHuidigeZasStatusAction} from "../actions/CervixDossierAction"
import {formatDateText} from "../utils/DateUtil"
import {showToast} from "../utils/ToastUtil"

export const getHuidigeZasStatus = () => async (dispatch: Dispatch<ResetZasStatusAction>) => {
	return ScreenitBackend.get("/cervix/zas/status")
		.then(response => dispatch(setHuidigeZasStatusAction(response.data)))
}

export const saveZasAanvraag = () => (dispatch: Dispatch) => {
	const verzendenUitstellen = false
	return ScreenitBackend.post(`/cervix/zas/aanvragen/${verzendenUitstellen}`)
		.then(() => {
			showToast(getString(properties.toast.title), getString(properties.toast.description.regulier))
		})
		.catch((error: AxiosResponse) => {
			if (error.status === HttpStatusCode.CONFLICT) {
				dispatch(createShowToastAction({description: getString(properties.error.zas_request), type: ToastMessageType.ERROR, alGetoond: false}))
			}
		})
}

export const saveZasAanvraagMetUitstel = (verzendenUitstellen: boolean, uitstellenTotDatum: Date | null) => (dispatch: Dispatch) => {
	return ScreenitBackend.post(`/cervix/zas/aanvragen/${verzendenUitstellen}`)
		.then(() => {
			showToast(getString(properties.toast.title),
				verzendenUitstellen ? getString(properties.toast.description.uitstel, [formatDateText(uitstellenTotDatum)]) :
					getString(properties.toast.description.regulier))
		})
		.catch((error: AxiosResponse) => {
			if (error.status === HttpStatusCode.CONFLICT) {
				dispatch(createShowToastAction({description: getString(properties.error.zas_request), type: ToastMessageType.ERROR, alGetoond: false}))
			}
		})
}
