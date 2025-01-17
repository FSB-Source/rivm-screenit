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
import Keycloak from "keycloak-js"
import {createContext, useEffect, useState} from "react"
import keycloak from "../utils/Keycloak"

interface KeycloakContextValue {
	initialized: boolean
	keycloak: Keycloak
}

export const KeycloakContext = createContext<KeycloakContextValue>({
	initialized: false,
	keycloak,
})

interface KeycloakProviderProps {
	onAuthRefreshError: () => void,
	children: React.ReactNode
}

const KeycloakProvider: React.FC<KeycloakProviderProps> = (props) => {
	const [initialized, setInitialized] = useState(false)

	keycloak.onAuthRefreshError = props.onAuthRefreshError
	keycloak.onReady = () => setInitialized(true)
	keycloak.onTokenExpired = () => {
		keycloak.updateToken(60).then(success => {
			if (!success) {
				props.onAuthRefreshError()
			}
		}).catch(props.onAuthRefreshError)
	}

	useEffect(() => {
		keycloak.init({
			checkLoginIframe: false,
			pkceMethod: "S256",
			onLoad: "check-sso",
		})
	}, [])

	return (
		<KeycloakContext.Provider value={{initialized, keycloak}}>
			{props.children}
		</KeycloakContext.Provider>
	)
}

export default KeycloakProvider
