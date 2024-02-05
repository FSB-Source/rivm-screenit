/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import styles from "./ActiveToastsComponent.module.scss"
import {Col, Row} from "react-bootstrap"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import SpanWithHtml from "../span/SpanWithHtml"

interface DefaultToastProps {
    index: number
    title?: string
    description: string
    clickClearButton: () => void
}

const DefaultToast = (props: DefaultToastProps) => {

    const {index, title, description, clickClearButton} = props
    return (
        <Row className={styles.toast} key={"toast_" + index}>
            <VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={34}/>
            <Col xs={10} className={styles.toastText}>
                <SpanWithHtml className={styles.title} value={title!}/>
                <SpanWithHtml className={styles.description} value={description!}/>
            </Col>
            <Col xs={2} className={styles.toastClear}
                 onClick={clickClearButton}>
                <i className="material-icons">clear</i>
            </Col>
        </Row>
    )

}

export default DefaultToast
