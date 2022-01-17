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
import {KandidaatAfspraak} from "../datatypes/mamma/KandidaatAfspraak"
import {createShowToastAction} from "../actions/ToastAction"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {getString} from "../utils/TekstPropertyUtil"
import {setHuidigeMammaAfspraakReduxAction} from "../actions/MammaDossierAction"
import properties from "../pages/bvo/mamma/afspraak/MammaAfspraakMakenPopup.json"
import {navigateAndShowToast} from "../utils/NavigationUtil"
import {getBvoBaseUrl} from "../utils/UrlUtil"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"

export const maakAfspraak = (bvo: Bevolkingsonderzoek, afspraak: KandidaatAfspraak) => (dispatch: Dispatch) => {
    return ScreenitBackend.post("/mamma/afspraak/maak", afspraak)
        .then(() => {
            navigateAndShowToast(getBvoBaseUrl(bvo), getString(properties.toast.title), afspraak.bevestigingsBrief ? getString(properties.toast.brief_melding) : getString(properties.toast.geen_brief))
        })
        .catch((error) => {
            if (error.response.data !== "tijd.niet.beschikbaar") {
                dispatch(createShowToastAction({
                    title: getString(properties.toast.geen_wijzigingen),
                    description: getString(properties.toast.error.algemeen),
                    type: ToastMessageType.ERROR,
                }))
            }
            return Promise.reject(error)
        })
}

export const getHuidigeAfspraak = () => (dispatch: Dispatch) => {
    return ScreenitBackend.get(`/mamma/afspraak/huidige`)
        .then(response => dispatch(setHuidigeMammaAfspraakReduxAction(response.data)))
}
