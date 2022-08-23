/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import styles from "./AanspreekVormIcon.module.scss"

const AanspreekVormIcon = () => {
	return (
		<svg className={styles.style} version="1.1" xmlns="http:
			 viewBox="0 0 48 48" xmlSpace="preserve">
			<g>
				<g>
					<polygon points="34.2,45.4 27.1,47.5 29.1,40 42.4,26.1 47.5,31.5"/>
					<line x1="38.3" y1="30.4" x2="43.4" y2="35.8"/>
					<line x1="29.1" y1="40" x2="34.2" y2="45.4"/>
				</g>
				<g>
					<polyline points="27.9,26.8 19.9,23.8 19.9,19.3"/>
					<path d="M11.7,19.3v4.3l-8,3c-2,0.9-3.3,2.8-3.3,4.9v3.2H22"/>
					<path d="M23.8,10.5c0,5.6-3.7,10-8,10c-4.5,0-8-4.5-8-10s3.7-10,8-10C20.3,0.5,23.8,5,23.8,10.5z"/>
					<path d="M23.8,9.7c-0.2,0-0.4,0-0.8,0c-2.7,0.6-4.5-0.4-5.9-3.2c-0.8,1.9-3.7,3.2-6.1,3.2c-1.2,0-2-0.2-3.1-0.9"/>
					<path d="M29.1,0.5c4.5,0,8,4.5,8,10s-3.7,10-8,10"/>
					<path d="M37.1,9.7c-0.2,0-0.4,0-0.8,0c-2.7,0.6-4.5-0.4-5.9-3.2"/>
					<line x1="33.2" y1="22.9" x2="33.2" y2="19.3"/>
				</g>
			</g>
		</svg>
	)
}

export default AanspreekVormIcon
