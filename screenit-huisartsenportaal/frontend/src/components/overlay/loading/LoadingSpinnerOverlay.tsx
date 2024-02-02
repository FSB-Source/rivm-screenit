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
import classNames from "classnames"
import styles from "./LoadingSpinner.module.scss"
import {Spinner} from "react-bootstrap"
import {useAppSelector} from "../../../index"
import React from "react"

const LoadingSpinnerOverlay = () => {
	const loading = useAppSelector(state => state.loading)

	return <div className={classNames(styles.style, loading && styles.visible)}>
		<div className={styles.spinnerContainer}>
			<Spinner className={styles.spinner} animation={"border"}/>
		</div>
	</div>
}

export default LoadingSpinnerOverlay
