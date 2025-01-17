/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import * as React from "react"
import {useContext} from "react"
import {Navigate, RouteProps} from "react-router-dom"
import {useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {KeycloakContext} from "../components/KeycloakProvider"

type PrivateRouteParams = RouteProps & {
	component: React.ComponentType<any>
}

export default function PrivateRoute({component: Component, ...rest}: PrivateRouteParams) {
	const {initialized, keycloak} = useContext(KeycloakContext)
	const authenticatie = useSelector((state: State) => state.authenticatie)

	const redirectComponent = (path: string) => {
		return <Navigate replace to={{pathname: path}}/>
	}

	if (initialized && keycloak.authenticated && authenticatie.isLoggedIn) {
		return authenticatie.isUnauthorized ? redirectComponent("/niet-in-bevolkingsonderzoek") : <Component {...rest} />
	} else {
		return redirectComponent("/login")
	}
}
