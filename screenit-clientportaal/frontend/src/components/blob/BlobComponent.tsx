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
import {useSelectedBvo} from "../../utils/Hooks"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import React from "react"
import styles from "./BlobComponent.module.scss"

export type BlobComponentProps = {
	children?: React.ReactNode
}

const BlobComponent = (props: BlobComponentProps) => {
	const selectedBvo = useSelectedBvo()

	return (
		<div className={styles.style}>
			<svg xmlns="http:
				 className={BevolkingsonderzoekStyle[selectedBvo!]}>
				<path
					d="M95.516,268.738A92.465,92.465,0,0,0,230.6,292.451c28.968-22.624,86.872-67.913,115.813-90.568A92.811,92.811,0,0,0,306.5,37.629C257.323,28.537,158.905,10.72,109.738,1.595A94.119,94.119,0,0,0,92.555,0,92.782,92.782,0,0,0,15.242,143.733c20.247,31.136,60.324,93.675,80.273,125"
					fill="#e3eaf1"/>
			</svg>
			{props.children && <div className={styles.content}>
				{props.children}
			</div>}
		</div>
	)
}

export default BlobComponent
