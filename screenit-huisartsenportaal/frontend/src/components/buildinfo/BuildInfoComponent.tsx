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
import React, {useEffect} from "react"
import styles from "./BuildInfoComponent.module.scss"
import {useAppSelector, useAppThunkDispatch} from "../../index"
import {fetchBuildInfo} from "../../api/BuildInfoThunkAction"
import {formatDateTime} from "../../util/DateUtil"

const BuildInfoComponent = () => {
	const buildInfo = useAppSelector(state => state.buildInfo)
	const dispatch = useAppThunkDispatch()

	useEffect(() => {
		if (!buildInfo) {
			dispatch(fetchBuildInfo())
		}
	}, [dispatch, buildInfo])

	return buildInfo && <div className={styles.style}>
		<span>ScreenIT - Huisartsenportaal {buildInfo.version} | {formatDateTime(new Date(buildInfo.timestamp))}<br/>Instance: {buildInfo.instance}</span>
	</div>
}

export default BuildInfoComponent
