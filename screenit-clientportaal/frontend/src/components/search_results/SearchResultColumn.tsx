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
import bvoStyles from "../BvoStyle.module.scss"
import styles from "./SearchResultColumn.module.scss"
import classNames from "classnames"
import SpanWithHtml from "../span/SpanWithHtml"

export type SearchResultColumnProps = {
    className?: string
    head?: string
    value1?: string
    value2?: string
    enlargeValue2?: boolean;
}

const SearchResultColumn = (props: SearchResultColumnProps) => {
    return (
        <div className={classNames(props.className, styles.content)}>
            <span className={classNames(bvoStyles.bvoText, styles.head)}>{props.head}</span>
            <SpanWithHtml value={props.value1 ? props.value1 : ""}/>
            {props.enlargeValue2 ? <SpanWithHtml className={styles.largeSpan} value={props.value2 ? props.value2 : ""}/> : <SpanWithHtml value={props.value2 ? props.value2 : ""}/>}
        </div>
    )
}

export default SearchResultColumn
