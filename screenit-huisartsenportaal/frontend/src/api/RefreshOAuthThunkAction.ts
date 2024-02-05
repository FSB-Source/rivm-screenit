/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {AppThunkDispatch} from "../index"
import ScreenitBackend from "../util/Backend"
import {AxiosResponse} from "axios"
import {OAuthToken} from "../state/datatypes/OAuthToken"
import {createActionSetOAuthToken} from "../state/OAuthState"

export const refreshOAuthThunkAction = (token: OAuthToken) => async (dispatch: AppThunkDispatch): Promise<OAuthToken> => {
	const formData = new FormData()
	formData.append("grant_type", "refresh_token")
	formData.append("refresh_token", token.refresh_token)
	formData.append("client_id", "screenit")
	formData.append("client_secret", "123456")
	formData.append("scope", token.scope)

	return await ScreenitBackend.post("/oauth/token",
		formData, {
			headers: {
				"Content-Type": "application/x-www-form-urlencoded",
			},
		}).then(async (response: AxiosResponse<OAuthToken>) => {
		const token = response.data
		dispatch(createActionSetOAuthToken(token))
		return token
	})
}
