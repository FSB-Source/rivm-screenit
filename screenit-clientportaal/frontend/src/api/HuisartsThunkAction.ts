/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {
	magMammaHuisartsOntkoppelenReduxAction,
	setMammaGeenHuisartsOptieHuidigeRondeReduxAction,
	setMammaGeenHuisartsOptieVorigeRondeReduxAction,
	setMammaHuisartsHuidigeRondeReduxAction,
	setMammaHuisartsVorigeRondeReduxAction,
} from "../actions/MammaDossierAction"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {Huisarts, MammaGeenHuisartsOptie} from "../datatypes/Huisarts"
import {setColonHuisartsHuidigeRondeReduxAction, setColonHuisartsVorigeRondeReduxAction} from "../actions/ColonDossierAction"
import {getBvoBaseUrl} from "../utils/UrlUtil"
import {assertUnreachable} from "../utils/EnumUtil"

export const koppelHuisarts = (huisarts: Huisarts, bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.post(`/huisarts${getBvoBaseUrl(bvo)}?id=${huisarts.id}`)
		.then(() => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(setMammaHuisartsHuidigeRondeReduxAction(huisarts))
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(setColonHuisartsHuidigeRondeReduxAction(huisarts))
					break
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}

export const ontkoppelHuisarts = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.delete(`/huisarts${getBvoBaseUrl(bvo)}`)
		.then(() => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(setMammaHuisartsHuidigeRondeReduxAction(undefined))
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(setColonHuisartsHuidigeRondeReduxAction(undefined))
					break
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}

export const magHuisartsKoppelen = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/huisarts${getBvoBaseUrl(bvo)}/magverwijderen`)
		.then(response => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(magMammaHuisartsOntkoppelenReduxAction(response.data))
					break
				case Bevolkingsonderzoek.COLON:
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}

export const getHuidigeHuisarts = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/huisarts${getBvoBaseUrl(bvo)}/huidige`)
		.then(response => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(setMammaHuisartsHuidigeRondeReduxAction(response.data))
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(setColonHuisartsHuidigeRondeReduxAction(response.data))
					break
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}

export const getVorigeHuisarts = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/huisarts${getBvoBaseUrl(bvo)}/vorige`)
		.then(response => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(setMammaHuisartsVorigeRondeReduxAction(response.data as Huisarts | undefined))
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(setColonHuisartsVorigeRondeReduxAction(response.data as Huisarts | undefined))
					break
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}

export const getHuidigeMammaGeenHuisartsOptie = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/huisarts${getBvoBaseUrl(bvo)}/huidige/geen`)
		.then(response => dispatch(setMammaGeenHuisartsOptieHuidigeRondeReduxAction(response.data)))
}

export const getVorigeMammaGeenHuisartsOptie = (bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.get(`/huisarts${getBvoBaseUrl(bvo)}/vorige/geen`)
		.then(response => dispatch(setMammaGeenHuisartsOptieVorigeRondeReduxAction(response.data)))
}

export const bevestigVorige = (vorigeHuisarts?: Huisarts, mammaVorigeGeenHuisartsOptie?: MammaGeenHuisartsOptie, bvo?: Bevolkingsonderzoek) => (dispatch: Dispatch) => {
	return ScreenitBackend.post(`/huisarts${getBvoBaseUrl(bvo)}/vorige`)
		.then(() => {
			switch (bvo) {
				case Bevolkingsonderzoek.MAMMA:
					dispatch(setMammaGeenHuisartsOptieHuidigeRondeReduxAction(mammaVorigeGeenHuisartsOptie))
					dispatch(setMammaHuisartsHuidigeRondeReduxAction(vorigeHuisarts))
					break
				case Bevolkingsonderzoek.COLON:
					dispatch(setColonHuisartsHuidigeRondeReduxAction(vorigeHuisarts))
					break
				case Bevolkingsonderzoek.CERVIX:
					break
				default:
					assertUnreachable(bvo)
			}
		})
}
