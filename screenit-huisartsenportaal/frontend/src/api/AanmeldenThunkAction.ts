/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {AppThunkDispatch} from "../index"
import ScreenitBackend from "../util/Backend"
import {createActionSetOAuthToken} from "../state/OAuthState"
import {OAuthToken} from "../state/datatypes/OAuthToken"
import {AxiosResponse} from "axios"
import {fetchCurrentUser} from "./CurrentUserThunkAction"
import {AuthenticationScope} from "../state/datatypes/enums/AuthenticationScope"
import {fetchHuisarts} from "./HuisartsThunkAction"
import {fetchLocaties, fetchLocatieVerificatie} from "./LocatieThunkAction"
import {LocatieStatus} from "../state/datatypes/dto/LocatieDto"
import {createActionSetAuthenticationLoading} from "../state/AuthenticationLoadingState"

export const authenticate = (username: string, password: string, scope: AuthenticationScope) => async (dispatch: AppThunkDispatch) => {
	const formData = new FormData()
	formData.append("client_id", "screenit")
	formData.append("grant_type", "password")
	formData.append("username", username)
	formData.append("password", password)
	formData.append("scope", scope)
	formData.append("client_secret", "123456")

	return ScreenitBackend.post("/oauth/token", formData, {
		headers: {
			"Content-Type": "multipart/form-data",
		},
	}).then(async (response: AxiosResponse<OAuthToken>) => {
		const token = response.data
		dispatch(createActionSetAuthenticationLoading(true))
		dispatch(createActionSetOAuthToken(token))
		await dispatch(fetchCurrentUser())
		dispatch(createActionSetAuthenticationLoading(false))
		await dispatch(fetchHuisarts())
		await dispatch(fetchLocatieVerificatie())
		await dispatch(fetchLocaties(LocatieStatus.ACTIEF))
	})
}
