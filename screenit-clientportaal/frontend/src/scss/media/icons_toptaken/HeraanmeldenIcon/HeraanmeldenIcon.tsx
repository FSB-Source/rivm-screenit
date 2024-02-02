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
import styles from "./HeraanmeldenIcon.module.scss"

const HeraanmeldenIcon = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <g>
                <circle className={styles.st0} cx="23.4" cy="23.4" r="22.2"/>
                <line className={styles.st0} x1="35.5" y1="23.3" x2="11.2" y2="23.4"/>
                <line className={styles.st0} x1="23.3" y1="35.5" x2="23.4" y2="11.2"/>
            </g>
        </svg>
    )
}

export default HeraanmeldenIcon
