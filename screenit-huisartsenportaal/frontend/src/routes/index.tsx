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
import React from "react"
import {Route, RouteProps, Routes} from "react-router"
import {routes} from "./routes"
import PrivateRoute from "./PrivateRoute"
import {AuthenticationScope} from "../state/datatypes/enums/AuthenticationScope"
import {Recht} from "../state/datatypes/enums/Recht"

export type RoutePath =
	"/"
	| "/betalingen"
	| "/gegevens"
	| "/overeenkomst"
	| "/verrichtingen"
	| "/login"
	| "/logout"
	| "/registreren"
	| "/registreren/voltooien"
	| "/wachtwoordvergeten"
	| "/wachtwoordvergeten/registreren"
	| "/wachtwoordvergeten/voltooien"

export type AppRoute = RouteProps & {
	path: RoutePath;
	scope?: AuthenticationScope;
	recht?: Recht;
	component: React.ComponentType<any>;
}

export const AppRoutes = () => {
	return (<Routes>
		{routes.map(route => {
			const Component = route.component
			return <Route key={route.path} path={route.path}
						  element={route.scope ? <PrivateRoute scope={route.scope} recht={route.recht} component={route.component}/> : <Component/>}/>
		})}
	</Routes>)
}
