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
import keycloak from "../utils/Keycloak"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import properties from "./backend.json"
import {transformDates} from "./DateTransformUtil"
import {showToast} from "./ToastUtil"
import httpStatus from "../datatypes/HttpStatus"
import {countRequest, countResponse} from "./SpinnerCounterUtil"
import axios, {AxiosError} from "axios"

const BASE_URL = "/api"

export const ScreenitBackend = axios.create({baseURL: BASE_URL})
ScreenitBackend.interceptors.request.use((config) => {
	countRequest()
	if (keycloak && keycloak.token !== undefined && config.headers) {
		config.headers.Authorization = `Bearer ${keycloak.token}`
	}
	return config
})

ScreenitBackend.interceptors.response.use((response) => {
	countResponse()
	response.data = transformDates(response.data)
	return response
}, (error: AxiosError) => {
	countResponse()
	if (error.response?.status !== httpStatus.NOT_MODIFIED && error.response?.status !== httpStatus.CONFLICT && error.response?.status !== httpStatus.NOT_FOUND) {
		showToast(undefined, properties.foutmelding, ToastMessageType.ERROR)
	}
	return Promise.reject(error)
})

export default ScreenitBackend
