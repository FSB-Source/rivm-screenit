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
import styles from "./LoadingSpinner.module.scss"
import {CircularProgress} from "@mui/material"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"

const LoadingSpinnerOverlay = () => {
	const requestMinusResponseCount = useSelector((state: State) => state.requestMinusResponseCounter)
	const spinnerNodig = requestMinusResponseCount !== 0

	return <div className={classNames(styles.style, spinnerNodig && styles.visible)}>
		<div className={styles.spinnerContainer}>
			<CircularProgress className={styles.spinner} size={60}/>
		</div>
	</div>
}

export default LoadingSpinnerOverlay
