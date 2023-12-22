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
import {useContext, useEffect} from "react"
import {useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {setLoggingInAction, setSessionExpiredAction} from "../actions/AuthenticatieAction"
import {processLogin} from "../api/LoginThunkAction"
import {createClearStateAction} from "../actions/RootAction"
import {useThunkDispatch} from "../index"
import {KeycloakContext} from "../components/KeycloakProvider"

export interface AuthenticationWrapperProps {
	children: JSX.Element;
}

const AuthenticationWrapper = (props: AuthenticationWrapperProps) => {
	const dispatch = useThunkDispatch()
	const {initialized: keycloakInitialized, keycloak} = useContext(KeycloakContext)
	const authenticatie = useSelector((state: State) => state.authenticatie)

	useEffect(() => {
		if (!keycloakInitialized) {
			return
		} else if (!keycloak.authenticated && authenticatie.isLoggingIn) {
			dispatch(setLoggingInAction(false))
			keycloak.login()
		} else if (keycloak.authenticated && !authenticatie.isLoggedIn) {
			dispatch(processLogin())
		} else if (keycloak.authenticated && (authenticatie.isLoggingOut || authenticatie.isSessionExpired)) {
			keycloak.logout()
		} else if (!keycloak.authenticated && authenticatie.isLoggedIn) {
			dispatch(createClearStateAction())
			if (authenticatie.isSessionExpired) {
				dispatch(setSessionExpiredAction(true))
			}
		}
	}, [dispatch, keycloakInitialized, keycloak, authenticatie])

	return props.children
}

export default AuthenticationWrapper
