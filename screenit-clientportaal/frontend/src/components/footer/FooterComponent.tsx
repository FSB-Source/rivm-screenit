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
import React from "react"
import {Col, Row} from "react-bootstrap"
import classNames from "classnames"
import styles from "./FooterComponent.module.scss"
import {getBevolkingsonderzoekNederlandUrl, getContactUrl} from "../../utils/UrlUtil"
import {useRegio} from "../../utils/Hooks"
import properties from "./FooterComponent.json"

const FooterComponent = () => {

    const regio = useRegio()

    return (
        <Row className={classNames(styles.footer, "footer", "align-items-center")}>
            <Col md={4}>
                <span className={styles.footerName}>{properties.title}</span>
            </Col>
            <Col md={8}>
                <ul className={styles.footerUrls}>
                    <a href={`${getBevolkingsonderzoekNederlandUrl()}/privacy/`}
                       rel="noopener noreferrer">
                        <li>{properties.links.privacy}</li>
                    </a>
                    <a href={`${getBevolkingsonderzoekNederlandUrl()}/responsible-disclosure/`}
                       rel="noopener noreferrer">
                        <li>{properties.links.responsible_disclosure}</li>
                    </a>
                    <a href={getContactUrl(regio)}
                       rel="noopener noreferrer">
                        <li>{properties.links.contact}</li>
                    </a>
                </ul>
            </Col>
        </Row>
    )

}

export default FooterComponent
