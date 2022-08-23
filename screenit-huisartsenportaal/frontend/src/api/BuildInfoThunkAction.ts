/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {AppThunkDispatch} from "../index"
import ScreenitBackend from "../util/Backend"
import {AxiosResponse} from "axios"
import {BuildDto} from "../state/datatypes/dto/BuildDto"
import {createActionSetBuildInfo} from "../state/BuildInfoState"

export const fetchBuildInfo = () => (dispatch: AppThunkDispatch) => {
	return ScreenitBackend.get("/build")
		.then((response: AxiosResponse<BuildDto>) => {
			dispatch(createActionSetBuildInfo(response.data))
		})
}
