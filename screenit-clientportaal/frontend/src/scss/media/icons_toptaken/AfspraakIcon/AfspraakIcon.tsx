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
import styles from "./AfspraakIcon.module.scss"

const AfspraakIcon = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <polyline className={styles.st0} points="40.4,19.3 40.4,4.6 32.5,4.6 "/>
            <polyline className={styles.st0} points="9,4.6 1.1,4.6 1.1,36 22.7,36 "/>
            <rect x="9" y="0.7" className={styles.st0} width="5.9" height="7.8"/>
            <rect x="26.7" y="0.7" className={styles.st0} width="5.9" height="7.8"/>
            <line className={styles.st0} x1="14.9" y1="4.6" x2="26.7" y2="4.6"/>
            <line className={styles.st0} x1="1.1" y1="14.4" x2="40.4" y2="14.4"/>
            <g>
                <polygon className={styles.st0} points="30.6,42.8 23.7,44.8 25.7,37.9 40.4,23.2 45.3,28.1 	"/>
                <line className={styles.st1} x1="36.5" y1="27.1" x2="41.4" y2="32"/>
                <line className={styles.st0} x1="25.7" y1="37.9" x2="30.6" y2="42.8"/>
            </g>
        </svg>
    )
}

export default AfspraakIcon
