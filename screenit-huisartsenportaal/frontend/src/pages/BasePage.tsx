/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import styles from "./BasePage.module.scss"
import classNames from "classnames"
import React from "react"

export interface BasePageProps {
	children: JSX.Element;
	title: string;
	description?: string;
}

const BasePage = (props: BasePageProps) => {
	return <div className={classNames(styles.style, "my-2", "px-2", "px-md-5", "py-4", "rounded")}>
		<h3 className={"px-2"}>{props.title}</h3>
		{props.description && <span className={"d-block mx-2"} dangerouslySetInnerHTML={{__html: props.description}}/>}
		{props.children}
	</div>
}

export default BasePage
