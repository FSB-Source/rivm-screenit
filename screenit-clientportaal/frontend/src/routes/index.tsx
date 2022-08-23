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
import {Navigate, Route, Routes} from "react-router-dom"
import NotFound from "../pages/NotFoundPage"
import {ClientContactActieType} from "../datatypes/ClientContactActieType"
import PrivateRoute from "./PrivateRoute"
import {useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {assertUnreachable} from "../utils/EnumUtil"
import routes, {RouteDef} from "./routes"
import {useKeycloak} from "@react-keycloak/web"
import LadenComponent from "../components/laden/LadenComponent"
import {LandingOverzicht} from "../datatypes/landing/LandingOverzicht"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"

export const AppRoutes = () => {
	const {initialized, keycloak} = useKeycloak()
	const authenticatie = useSelector((state: State) => state.authenticatie)

	const contactActions = useSelector((state: State) => state.client.beschikbareActies.beschikbareActies)
	const landingOverzicht = useSelector((state: State) => state.landingOverzicht)

	if (!initialized || authenticatie.isLoggingOut || (!authenticatie.isLoggedIn && keycloak.authenticated)) {
		return <LadenComponent groteText/>
	}

	return (
		<Routes>
			{routes.map(route => createRoute(route, contactActions, landingOverzicht))}
			<Route path={"*"} element={<NotFound/>}/>
		</Routes>
	)
}

function createRoute(route: RouteDef, contactActions: ClientContactActieType[], landingOverzicht: LandingOverzicht) {
	const {requiredContactActions, bvo} = route

	if (bvo && !behoortTotDoelgroep(bvo, landingOverzicht)) {
		return redirect(route)
	}
	if (requiredContactActions && isNotAllowed(requiredContactActions, contactActions)) {
		return redirect(route)
	}
	return createPublicOrPrivateRoute(route)
}

function createPublicOrPrivateRoute(routeDef: RouteDef): JSX.Element {
	const Component = routeDef.component
	return <Route key={routeDef.name} path={routeDef.path} element={routeDef.private ? <PrivateRoute component={routeDef.component}/> : <Component/>}/>
}

function isNotAllowed(requiredActions: ClientContactActieType[], availableActions: ClientContactActieType[]): boolean {
	return requiredActions
		.map(action => availableActions.indexOf(action) > -1)
		.filter(allowed => allowed)
		.length === 0
}

function behoortTotDoelgroep(bvo: Bevolkingsonderzoek, landingOverzicht: LandingOverzicht): boolean {
	switch (bvo) {
		case Bevolkingsonderzoek.MAMMA:
			return landingOverzicht.behoortTotMammaDoelgroep
		case Bevolkingsonderzoek.CERVIX:
			return landingOverzicht.behoortTotCervixDoelgroep
		case Bevolkingsonderzoek.COLON:
			return landingOverzicht.behoortTotColonDoelgroep
		default:
			assertUnreachable(bvo)
	}
	return false
}

function redirect(route: RouteDef) {
	const {name, redirectPage} = route
	return <Route key={name} {...{...route, component: undefined}}
				  element={<Navigate to={redirectPage ?? "/"}/>}/>
}
