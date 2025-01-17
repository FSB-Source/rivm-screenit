/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {AppThunkDispatch} from "../index"
import ScreenitBackend, {validatingRequest} from "../util/Backend"
import {AxiosResponse} from "axios"
import {createActionSetHuisarts} from "../state/HuisartsState"
import {HuisartsDto} from "../state/datatypes/dto/HuisartsDto"

export const fetchHuisarts = () => async (dispatch: AppThunkDispatch) => {
	const huisarts = await ScreenitBackend.get("/huisarts")
		.then((response: AxiosResponse<HuisartsDto>) => {
			return response.data
		})
	if (huisarts) {
		dispatch(createActionSetHuisarts(huisarts))
	}
}

export const controleerHuisarts = (huisarts: HuisartsDto) => async (dispatch: AppThunkDispatch): Promise<HuisartsDto> => {
	return await dispatch(validatingRequest<HuisartsDto>("/huisarts/controle", "PUT", huisarts)).then((response) => {
		return response
	})
}

export const saveHuisarts = (huisarts: HuisartsDto) => async (dispatch: AppThunkDispatch): Promise<HuisartsDto> => {
	return await dispatch(validatingRequest<HuisartsDto>("/huisarts", "PUT", huisarts)).then((response) => {
		dispatch(createActionSetHuisarts(response))
		return response
	})
}
