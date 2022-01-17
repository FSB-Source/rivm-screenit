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
import React from "react"
import {Link} from "react-router-dom"
import {useKeycloak} from "@react-keycloak/web"
import properties from "./NotFoundPage.json"
import SpanWithHtml from "../components/span/SpanWithHtml"

const NotFoundPage = () => {

    const {keycloak} = useKeycloak()

    return keycloak?.authenticated ?
        (
            <div>
                <h1>{properties.title}</h1>
                <SpanWithHtml value={properties.authenticated.message}/>
                <Link to="/">{properties.authenticated.link_text}</Link>
            </div>
        ) : (
            <div>
                <h1>{properties.title}</h1>
                <SpanWithHtml value={properties.unauthenticated.message}/>
                <Link to="/login">{properties.unauthenticated.link_text}</Link>
            </div>
        )

}

export default NotFoundPage
