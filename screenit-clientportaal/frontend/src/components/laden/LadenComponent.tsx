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
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./Laden.json"
import styles from "./LadenComponent.module.scss"
import classNames from "classnames"

export interface LadenComponentProps {
	groteText?: boolean;
}

const LadenComponent = (props: LadenComponentProps) => {
	return (
		<h1 className={classNames(!props.groteText && styles.kleineText)}>{getString(properties.laden)}</h1>
	)
}

export default LadenComponent
