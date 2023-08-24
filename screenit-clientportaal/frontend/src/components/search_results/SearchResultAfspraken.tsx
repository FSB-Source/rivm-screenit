/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import SearchResultColumn from "./SearchResultColumn"
import styles from "./SearchResultAfspraken.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import classNames from "classnames"
import ArrowIconComponent, {ArrowType} from "../vectors/ArrowIconComponent"
import {Col, Row} from "react-bootstrap"

export type SearchResultProps = {
    className?: string
    col1?: string[]
    col2?: string[]
    col3?: string[]
    onHoverText?: string
    enlargeText?: boolean
    onClickAction: () => void
}

const SearchResultAfspraken = (props: SearchResultProps) => {
    return (
        <div className={classNames(props.className, styles.content)}
             onClick={(event) => {
                 event.preventDefault();
                 props.onClickAction();
             }}>
            <Row>
				<Col sm={4} className={styles.col}>
					{props.col1 && <SearchResultColumn
						head={props.col1[0]}
						value1={props.col1[1]}
						value2={props.col1[2]}
						enlargeValue2={props.enlargeText === undefined ? true : props.enlargeText}/>}
				</Col>
				<Col sm={4} className={styles.col}>
					{props.col2 && <SearchResultColumn
						head={props.col2[0]}
						value1={props.col2[1]}
						value2={props.col2[2]}
					/>}
				</Col>
				<Col sm={4} className={styles.col}>
					{props.col3 && <SearchResultColumn
						head={props.col3[0]}
						value1={props.col3[1]}
						value2={props.col3[2]}
					/>}
				</Col>
				<Col sm={4} className={classNames(styles.hoverOverlay, bvoStyle.baseBackgroundColor)}>
					<div className={styles.hoverOverlayTextarea}>
						<span>{props.onHoverText}</span><ArrowIconComponent className={styles.arrow} type={ArrowType.ARROW_RIGHT}/>
					</div>
				</Col>
			</Row>
        </div>
    )
}

export default SearchResultAfspraken
