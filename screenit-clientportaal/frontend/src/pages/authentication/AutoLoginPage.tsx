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
import {Redirect} from "react-router-dom"
import {useKeycloak} from "@react-keycloak/web"
import {useDispatch, useSelector} from "react-redux"
import {Col, Row} from "react-bootstrap"
import {State} from "../../datatypes/State"
import {setLoggingInAction} from "../../actions/AuthenticatieAction"
import properties from "./AutoLoginPage.json"
import {getString} from "../../utils/TekstPropertyUtil"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import {procesLoginAndLogBrowserInfo} from "../../utils/LoginUtil"

const AutoLoginPage = () => {
    const {initialized: keycloakInitialized, keycloak} = useKeycloak()
    const dispatch = useDispatch()
    const authenticatie = useSelector((state: State) => state.authenticatie)

    useEffect(() => {
        procesLoginAndLogBrowserInfo(keycloakInitialized, keycloak, dispatch, authenticatie)
    }, [keycloakInitialized, keycloak, dispatch, authenticatie])

    useEffect(() => {
        if (keycloakInitialized) {
            dispatch(setLoggingInAction(true))
            setTimeout(() => {
                keycloak.login()
            }, 3000)
        }
    }, [keycloakInitialized, keycloak, dispatch])

    if (keycloak?.authenticated && authenticatie.isLoggedIn && !authenticatie.isLoggingOut) {
        return <Redirect to={"/"}/>
    }
    return (
        <div>
            <Row>
                <Col sm={12}>
                    {getString(properties.auto)} <a href={getBevolkingsonderzoekNederlandUrl()}><span>{getBevolkingsonderzoekNederlandUrlNaam()}</span></a>
                </Col>
            </Row>
        </div>
    )
}

export default AutoLoginPage
