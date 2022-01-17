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
import axios, {AxiosError} from "axios"
import keycloak from "../utils/Keycloak"
import {cpStore} from "../index"
import {createShowToastAction} from "../actions/ToastAction"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import properties from "./backend.json"
import HttpStatusCode from "../datatypes/HttpStatus"
import {transformDates} from "./DateTransformUtil"

const BASE_URL = "/api"

export const ScreenitBackend = axios.create({baseURL: BASE_URL})

ScreenitBackend.interceptors.request.use((config) => {
	if (keycloak && keycloak.token !== undefined && config.headers) {
		config.headers.Authorization = `Bearer ${keycloak.token}`
	}
	return config
})

ScreenitBackend.interceptors.response.use((response) => {
	response.data = transformDates(response.data)
	return response
}, (error: AxiosError) => {
	if (error.response) {
		if (error.response.status === HttpStatusCode.FORBIDDEN) {
			cpStore.dispatch(createShowToastAction({
				type: ToastMessageType.ERROR,
				description: properties.foutmelding,
			}))
		}
		if (error.response.status >= HttpStatusCode.INTERNAL_SERVER_ERROR) {
			cpStore.dispatch(createShowToastAction({
				type: ToastMessageType.ERROR,
				description: properties.foutmelding,
			}))
		}
	} else {
		cpStore.dispatch(createShowToastAction({
			type: ToastMessageType.ERROR,
			description: properties.foutmelding,
		}))
	}
	return Promise.reject(error)
})

export default ScreenitBackend
