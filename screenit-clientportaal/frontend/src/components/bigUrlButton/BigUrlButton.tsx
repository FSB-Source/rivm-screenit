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
import classNames from "classnames"
import styles from "./BigUrlButton.module.scss"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import bvoStyle from "../BvoStyle.module.scss"
import {useSelectedBvo} from "../../utils/Hooks"
import {isExternalUrl} from "../../utils/UrlUtil"
import {useNavigate} from "react-router-dom"

export type BigUrlButtonProps = {
	title: string,
	text: string,
	link: string,
}

const BigUrlButton = (props: BigUrlButtonProps) => {
	const bvo = useSelectedBvo()
	const navigate = useNavigate()

	return (
		<div className={classNames(styles.bigUrl, BevolkingsonderzoekStyle[bvo!])}
			 onClick={() => isExternalUrl(props.link) ? window.open(props.link, "_self") : navigate(props.link)}>
			<span className={bvoStyle.bvoText}>{props.title}</span>
			<br/>
			<span>{props.text}</span>
		</div>
	)
}

export default BigUrlButton
