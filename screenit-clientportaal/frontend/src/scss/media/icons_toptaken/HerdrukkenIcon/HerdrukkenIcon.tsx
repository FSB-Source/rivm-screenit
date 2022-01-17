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
import styles from "./HerdrukkenIcon.module.scss"

const HerdrukkenIcon = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 39 47" xmlSpace="preserve">
            <g>
                <path className={styles.st0}
                      d="M38.5,43.5c0,1.7-1.3,3-3,3h-32c-1.7,0-3-1.3-3-3v-18c0-1.7,1.3-3,3-3h32c1.7,0,3,1.3,3,3V43.5z"/>
                <polyline className={styles.st0} points="37.6,23.4 19.5,37.5 1.4,23.3 	"/>
                <polyline className={styles.st0} points="28.5,1.5 28.5,6.5 23.5,6.5 	"/>
                <path className={styles.st0} d="M10.5,6.5c1.2-3.5,5.1-6,9-6s7.8,2.5,9,6"/>
                <polyline className={styles.st0} points="10.5,17.5 10.5,12.5 15.5,12.5 	"/>
                <path className={styles.st0} d="M28.5,12.5c-1.2,3.5-5.1,6-9,6s-7.8-2.5-9-6"/>
            </g>
        </svg>
    )
}

export default HerdrukkenIcon
