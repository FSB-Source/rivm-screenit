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
import React, {useEffect} from "react"
import classNames from "classnames"
import authenticationStyle from "./AuthenticationPage.module.scss"
import styles from "./LogoutPage.module.scss"
import {Col, Row} from "react-bootstrap"
import ImageBlobComponent from "../../components/blob/ImageBlobComponent"
import blob_personen from "../../scss/media/blob-personen.jpg"
import {getString} from "../../utils/TekstPropertyUtil"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import {useRedirectAfterSeconds} from "../../utils/Hooks"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import properties from "./LogoutPage.json"
import {useKeycloak} from "@react-keycloak/web"
import {useDispatch, useSelector} from "react-redux"
import {logout} from "../../utils/LogoutUtil"
import {State} from "../../datatypes/State"

const LogoutPage = () => {

    const {keycloak} = useKeycloak()
    const dispatch = useDispatch()
    const authenticatie = useSelector((state: State) => state.authenticatie)

    useEffect(() => {
        if (!authenticatie.isLoggingOut) {
            logout(keycloak, dispatch, false)
        }
    }, [authenticatie, keycloak, dispatch])

    useRedirectAfterSeconds(getBevolkingsonderzoekNederlandUrl(), 5)

    return <div className={classNames(authenticationStyle.style, styles.style)}>
        <Row>
            <Col sm={8}>
                <h1>{getString(properties.page.title)}</h1>
                <div className={authenticationStyle.introTextContainer}>
                    <SpanWithHtml value={getString(properties.page.body, [getBevolkingsonderzoekNederlandUrl(), getBevolkingsonderzoekNederlandUrlNaam()])}/>
                </div>
            </Col>
            <Col sm={4}>
                <ImageBlobComponent image={blob_personen} className={authenticationStyle.blob}/>
            </Col>
        </Row>
    </div>

}

export default LogoutPage
