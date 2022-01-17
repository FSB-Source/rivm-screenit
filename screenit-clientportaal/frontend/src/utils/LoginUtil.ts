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
import {setLoggedInAction, setLoggingInAction} from "../actions/AuthenticatieAction"
import {getUA} from "react-device-detect"
import {Dispatch} from "redux"
import {KeycloakInstance} from "keycloak-js"
import {AuthenticatieState} from "../datatypes/authenticatie/AuthenticatieState"
import ScreenitBackend from "./Backend"
import {navigate} from "../routes/routes"
import {AxiosError} from "axios"
import HttpStatusCode from "../datatypes/HttpStatus"

export const procesLoginAndLogBrowserInfo = (keycloakInitialized: boolean, keycloak: KeycloakInstance, dispatch: Dispatch<any>, authenticatie: AuthenticatieState) => {
    if (keycloakInitialized && keycloak?.authenticated && authenticatie.isLoggingIn && !authenticatie.isLoggedIn) {
        ScreenitBackend.put("/login", {
            userAgent: getUA,
        }).then(() => {
            dispatch(setLoggingInAction(false))
        }).catch((error: AxiosError) => {
            if (error?.response?.status === HttpStatusCode.UNAUTHORIZED) {
                navigate("/niet-in-bevolkingsonderzoek")
            }
        }).finally(() => {
            dispatch(setLoggedInAction(true))
        })
    }
}
