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
import styles from "./EmailadresWijzigenIcon.module.scss"

const EmailadresWijzigenIcon = () => {
	return (
		<svg version="1.1" id="Layer_1" xmlns="http:
			 xmlSpace="preserve">
			<g>
				<path className={styles.mailicon}
					  d="M40.8,30.1c-0.6,1.6-2.5,2.9-4.1,2.9H13.2c-1.6,0-2.3-1.4-1.8-2.9L17,16.4c0.6-1.6,2.5-2.9,4.1-2.9h23.5 c1.6,0,2.3,1.4,1.8,2.9L40.8,30.1z"/>
				<polyline className={styles.mailicon} points="42,17.4 28.1,25.2 20.5,17.4"/>
				<line className={styles.mailicon} x1="15.4" y1="30.1" x2="22.2" y2="25.2"/>
				<line className={styles.mailicon} x1="36.9" y1="30.1" x2="34" y2="25.2"/>
				<line className={styles.mailicon} x1="11.3" y1="13.5" x2="1.5" y2="13.5"/>
				<line className={styles.mailicon} x1="5.4" y1="31.1" x2="1.5" y2="31.1"/>
				<line className={styles.mailicon} x1="7.4" y1="21.3" x2="1.5" y2="21.3"/>
			</g>
		</svg>
	)
}

export default EmailadresWijzigenIcon
