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
import styles from "./ActiveToastsComponent.module.scss"
import {Col, Row} from "react-bootstrap"
import classNames from "classnames"
import SpanWithHtml from "../span/SpanWithHtml"

interface ErrorToastProps {
    index: number
    title?: string
    description: string
    clickClearButton: () => void
}

const ErrorToast = (props: ErrorToastProps) => {
    const {index, title, description, clickClearButton} = props
    return (
        <Row className={classNames(styles.toast, styles.error)} key={"toast_" + index}>
			<Col xs={10} className={styles.toastText}>
				<i className={classNames("material-icons")}>warning</i>
				<SpanWithHtml className={styles.title} value={title || ""}/>
				<SpanWithHtml className={styles.description} value={description}/>
			</Col>
			<Col xs={2} className={classNames(styles.toastClear, styles.error)}
				 onClick={clickClearButton}>
				<i className={classNames("material-icons", styles.error, "zwart")}>clear</i>
			</Col>
		</Row>
    )
}

export default ErrorToast
