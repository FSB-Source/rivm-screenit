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
import style from "./BasePopup.module.scss"
import React from "react"
import classNames from "classnames"
import {isEmpty} from "../../utils/EmptyUtil"
import SpanWithHtml from "../span/SpanWithHtml"

export type BasePopupProps = {
	title: string,
	description: string,
	children: React.ReactNode
}

const BasePopup = (props: BasePopupProps) => {
	return (
		<div className={classNames(style.overlay, !isEmpty(props) && style.visible)}>
			<div className={classNames(style.content)}>
				<h2>{props.title}</h2>

				<SpanWithHtml value={props.description}/>

				<div className={style.description}>
					{props.children}
				</div>
			</div>
        </div>
    )
}

export default BasePopup
