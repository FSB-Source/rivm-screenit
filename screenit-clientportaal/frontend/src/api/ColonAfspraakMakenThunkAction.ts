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
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {getString} from "../utils/TekstPropertyUtil"
import {VrijSlotZonderKamer} from "../pages/bvo/colon/afspraak/ColonAfspraakMakenPage"
import {ExceptieOmschrijvingDto} from "../datatypes/ExceptieOmschrijvingDto"
import HttpStatusCode from "../datatypes/HttpStatus"
import properties from "../pages/bvo/colon/afspraak/ColonAfspraakMakenBevestigingsPopup.json"
import {showToast} from "../utils/ToastUtil"

export const afspraakVerplaatsen = (nieuwafspraak: VrijSlotZonderKamer, onError: () => void) => (dispatch: Dispatch) => {
	return voerActieUit("/colon/afspraak/verplaatsen", nieuwafspraak, dispatch, onError)
}

export const nieuweAfspraak = (nieuwafspraak: VrijSlotZonderKamer, onError: () => void) => (dispatch: Dispatch) => {
	return voerActieUit("/colon/afspraak/maken", nieuwafspraak, dispatch, onError)
}

function voerActieUit(backendUrl: string, nieuwafspraak: VrijSlotZonderKamer, dispatch: Dispatch, onError: () => void) {
	return ScreenitBackend.put(backendUrl, nieuwafspraak)
		.then(() => {
			showToast(getString(properties.toast.bevestiging.title), getString(properties.toast.bevestiging.message))
		})
		.catch((error) => {
			if (error.response.status === HttpStatusCode.CONFLICT) {
				setExceptieOmschrijvingDto(error.response.data)
			}
			onError()
		})
}

function setExceptieOmschrijvingDto(data: string) {
	const propertiesExceptie = require("../pages/bvo/colon/afspraak/ColonAfspraakMakenBevestigingsPopup.json")
	const jsonOutput: ExceptieOmschrijvingDto = JSON.parse(data)

	showToast(getString(propertiesExceptie.toast.errors.verplaatsen[jsonOutput.keyValue]), jsonOutput.additionalMessage, ToastMessageType.ERROR)
}
