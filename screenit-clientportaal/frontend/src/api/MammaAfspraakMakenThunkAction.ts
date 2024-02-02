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
import {KandidaatAfspraak} from "../datatypes/mamma/KandidaatAfspraak"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {getString} from "../utils/TekstPropertyUtil"
import {setHuidigeMammaAfspraakReduxAction} from "../actions/MammaDossierAction"
import properties from "../pages/bvo/mamma/afspraak/bevestigingswizard/wizard_componenten/MammaAfspraakMakenWizardModuleProperties.json"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {showToast} from "../utils/ToastUtil"
import {AxiosResponse} from "axios"

export const maakAfspraak = (bvo: Bevolkingsonderzoek, afspraak: KandidaatAfspraak): () => Promise<AxiosResponse<string>> => () => {
	return ScreenitBackend.post("/mamma/afspraak/maak", afspraak)
		.then(response => {
			return response
		})
		.catch((error) => {
			if (error.response.data !== "tijd.niet.beschikbaar") {
				showToast(getString(properties.afspraak_maken.toast.geen_wijzigingen), getString(properties.afspraak_maken.toast.error.algemeen), ToastMessageType.ERROR)
			}
			return Promise.reject(error)
		})
}

export const getHuidigeAfspraak = () => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/mamma/afspraak/huidige`)
		.then(response => dispatch(setHuidigeMammaAfspraakReduxAction(response.data)))
}
