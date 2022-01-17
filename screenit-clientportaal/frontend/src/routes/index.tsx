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
import {Redirect, Route, Switch} from "react-router-dom"
import NotFound from "../pages/NotFoundPage"
import {ClientContactActieType} from "../datatypes/ClientContactActieType"
import PrivateRoute from "./PrivateRoute"
import {useSelector} from "react-redux"
import {State} from "../datatypes/State"
import {assertUnreachable} from "../utils/EnumUtil"
import routes, {Doelgroep, RouteDef} from "./routes"

export const AppRouter = () => {
    const contactActions = useSelector((state: State) => state.client.beschikbareActies.beschikbareActies)
    const doelgroepen = {
        mamma: useSelector((state: State) => state.landingOverzicht.clientBehoortTotMammaDoelgroep),
        colon: useSelector((state: State) => state.landingOverzicht.clientBehoortTotColonDoelgroep),
        cervix: useSelector((state: State) => state.landingOverzicht.clientBehoortTotCervixDoelgroep),
    }

    return (
        <Switch>
            {routes.map(route => createRoute(route, contactActions, doelgroepen))}
            <Route component={NotFound}/>
        </Switch>
    )
}

function createRoute(route: RouteDef, contactActions: ClientContactActieType[], doelgroepen: { mamma: boolean, colon: boolean, cervix: boolean }) {

    const {requiredContactActions, doelgroep} = route

    if (doelgroep && behoortNietTotDoelgroep(doelgroep, doelgroepen)) {
        return redirect(route)
    }

    if (requiredContactActions && isNotAllowed(requiredContactActions, contactActions)) {
        return redirect(route)

    }

    return createPublicOrPrivateRoute(route)

}

function createPublicOrPrivateRoute(routeDef: RouteDef): JSX.Element {
    return routeDef.private ?
        <PrivateRoute key={routeDef.name} {...routeDef}/> :
        <Route key={routeDef.name} {...routeDef}/>
}

function isNotAllowed(requiredActions: ClientContactActieType[], availableActions: ClientContactActieType[]): boolean {
    return requiredActions
        .map(action => availableActions.indexOf(action) > -1)
        .filter(allowed => allowed)
        .length === 0
}

function behoortNietTotDoelgroep(doelgroep: Doelgroep, doelgroepen: { mamma: boolean, colon: boolean, cervix: boolean }): boolean {
    switch (doelgroep) {
        case "cervix":
            return !doelgroepen.cervix
        case "colon":
            return !doelgroepen.colon
        case "mamma":
            return !doelgroepen.mamma
        default:
            assertUnreachable(doelgroep)
    }
    return false
}

function redirect(route: RouteDef) {
    const {name, redirectPage} = route
    return <Route key={name} {...{...route, component: undefined}}
                  render={() => <Redirect to={redirectPage ?? "/"}/>}/>
}
