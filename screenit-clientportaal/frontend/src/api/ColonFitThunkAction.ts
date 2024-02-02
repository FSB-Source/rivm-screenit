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
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {getString} from "../utils/TekstPropertyUtil"
import {AxiosResponse} from "axios"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {ResetFitStatus, setHuidigeFitStatusAction} from "../actions/ColonDossierAction"
import properties from "../pages/bvo/colon/ColonFitAanvragenPage.json"
import {showToast} from "../utils/ToastUtil"

export const getHuidigeFitStatus = () => (dispatch: Dispatch<ResetFitStatus>) => {
	return ScreenitBackend.get("/colon/fit/status")
		.then(response => dispatch(setHuidigeFitStatusAction(response.data)))
}

export const saveFitAanvraag = () => () => {
	return ScreenitBackend.put("/colon/fit/aanvragen")
		.then(() => {
			showToast(getString(properties.toast.title), getString(properties.toast.description))
		})
		.catch((error: AxiosResponse) => {
			if (error.status === 409) {
				showToast(undefined, getString(properties.error.request), ToastMessageType.ERROR)
			}
		})
}
