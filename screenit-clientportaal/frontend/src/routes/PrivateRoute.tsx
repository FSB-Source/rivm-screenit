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
import * as React from "react"
import {useEffect} from "react"
import type {RouteProps} from "react-router-dom"
import {Redirect, Route, RouteComponentProps} from "react-router-dom"
import {useKeycloak} from "@react-keycloak/web"
import {useDispatch, useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {setLoggedInAction, setLoggingOutAction, setSessionExpiredAction} from "../actions/AuthenticatieAction"
import {createClearStateAction} from "../actions/RootAction"
import LadenComponent from "../components/laden/LadenComponent"

interface PrivateRouteParams extends RouteProps {
    component:
        | React.ComponentType<RouteComponentProps<any>>
        | React.ComponentType<any>
}

export default function PrivateRoute({component: Component, ...rest}: PrivateRouteParams) {
    const {initialized, keycloak} = useKeycloak()
    const dispatch = useDispatch()
    const authenticatie = useSelector((state: State) => state.authenticatie)

    useEffect(() => {
        if (initialized) {
			if (keycloak.authenticated && !authenticatie.isLoggingIn && !authenticatie.isLoggingOut && !authenticatie.isLoggedIn && !authenticatie.isSessionExpired) {
				dispatch(setLoggedInAction(true))
			} else if (!keycloak.authenticated && (authenticatie.isLoggingOut || authenticatie.isLoggedIn)) {
				dispatch(createClearStateAction())
				if (!authenticatie.isLoggingOut) {
					dispatch(setSessionExpiredAction(true))
				} else {
					dispatch(setLoggingOutAction(false))
				}
			}
		}
    }, [initialized, keycloak.authenticated, dispatch, authenticatie.isLoggingOut, authenticatie.isLoggedIn, authenticatie.isLoggingIn, authenticatie.isSessionExpired])

    const redirectComponent = (path: string) => {
        return <Redirect
            to={{pathname: path}}
        />
    }

    return (
        <Route{...rest} render={(props) => {
            if (!initialized) {
                return <LadenComponent/>
            } else if (keycloak?.authenticated && authenticatie.isLoggedIn) {
                return <Component {...props} />
            } else {
                return redirectComponent("/login")
            }
        }}/>
    )
}
